/* GLIB - Library of useful routines for C programming
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GLib Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GLib Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GLib at ftp://ftp.gtk.org/pub/gtk/.
 */

using System;
using System.Runtime.InteropServices;

namespace MHash {

struct MHashNode
{
	public IntPtr   key;
	public IntPtr   value;

	/* If key_hash == 0, node is not in use
	 * If key_hash == 1, node is a tombstone
	 * If key_hash >= 2, node contains data */
	public uint      key_hash;
};

public delegate bool MHRFunc  (IntPtr  key,
			       IntPtr  value,
			       IntPtr  user_data);

public delegate uint MHashFunc (IntPtr key);
public delegate bool MEqualFunc (IntPtr a, IntPtr b);
public delegate void MDestroyNotify (IntPtr data);
public delegate void MHFunc (IntPtr key,
			     IntPtr value,
			     IntPtr user_data);


[StructLayout (LayoutKind.Sequential)]
public unsafe struct MHashTable {

	const int HASH_TABLE_MIN_SHIFT = 3;  /* 1 << 3 == 8 buckets */

	int             size;
	int             mod;
	uint            mask;
	int             nnodes;
	int             noccupied;  /* nnodes + tombstones */
	MHashNode*      nodes;
	IntPtr          hash_func;
	IntPtr          key_equal_func;
	int             ref_count;
	int             version;
	MDestroyNotify  key_destroy_func;
	MDestroyNotify  value_destroy_func;

	/* Each table size has an associated prime modulo (the first prime
	 * lower than the table size) used to find the initial bucket. Probing
	 * then works modulo 2^n. The prime modulo is necessary to get a
	 * good distribution with poor hash functions. */
	static int[] prime_mod = {
		1,          /* For 1 << 0 */
		2,
		3,
		7,
		13,
		31,
		61,
		127,
		251,
		509,
		1021,
		2039,
		4093,
		8191,
		16381,
		32749,
		65521,      /* For 1 << 16 */
		131071,
		262139,
		524287,
		1048573,
		2097143,
		4194301,
		8388593,
		16777213,
		33554393,
		67108859,
		134217689,
		268435399,
		536870909,
		1073741789,
		2147483647  /* For 1 << 31 */
	};

	private void SetShift (int shift)
	{
		mask = 0;

		size = 1 << shift;
		mod  = prime_mod [shift];

		for (int i = 0; i < shift; i++) {
			mask <<= 1;
			mask |= 1;
		}
	}

	private static int FindClosestShift (int n)
	{
		int i;

		for (i = 0; n != 0; i++)
			n >>= 1;

		return i;
	}

	private void SetShiftFromSize (int size)
	{
		int shift;

		shift = FindClosestShift (size);
		shift = Math.Max (shift, HASH_TABLE_MIN_SHIFT);

		SetShift (shift);
	}

	/*
	 * m_hash_table_lookup_node:
	 * @hash_table: our #MHashTable
	 * @key: the key to lookup against
	 * @hash_return: optional key hash return location
	 * Return value: index of the described #MHashNode
	 *
	 * Performs a lookup in the hash table.  Virtually all hash operations
	 * will use this function internally.
	 *
	 * This function first computes the hash value of the key using the
	 * user's hash function.
	 *
	 * If an entry in the table matching @key is found then this function
	 * returns the index of that entry in the table, and if not, the
	 * index of an empty node (never a tombstone).
	 */
	private uint LookupNode (IntPtr  key)
	{
		MHashNode *node;
		uint node_index;
		uint hash_value;
		uint step = 0;

		/* Empty buckets have hash_value set to 0, and for tombstones, it's 1.
		 * We need to make sure our hash value is not one of these. */

		MHashFunc hfunc = (MHashFunc)Marshal.GetDelegateForFunctionPointer (hash_func, typeof (MHashFunc));

		hash_value = hfunc (key);
		if (hash_value <= 1)
			hash_value = 2;

		node_index = (uint)(hash_value % mod);
		node = &nodes [node_index];

		while (node->key_hash != 0) {
			/*  We first check if our full hash values
			 *  are equal so we can avoid calling the full-blown
			 *  key equality function in most cases.
			 */
			if (node->key_hash == hash_value) {
				if (key_equal_func != IntPtr.Zero) {
					MEqualFunc equalfunc = (MEqualFunc)Marshal.GetDelegateForFunctionPointer (key_equal_func, typeof (MEqualFunc));

					if (equalfunc (node->key, key))
						break;
				}
				else if (node->key == key) {
					break;
				}
			}

			step++;
			node_index += step;
			node_index &= mask;
			node = &nodes [node_index];
		}

		return node_index;
	}

	/*
	 * m_hash_table_lookup_node_for_insertion:
	 * @hash_table: our #MHashTable
	 * @key: the key to lookup against
	 * @hash_return: key hash return location
	 * Return value: index of the described #MHashNode
	 *
	 * Performs a lookup in the hash table, preserving extra information
	 * usually needed for insertion.
	 *
	 * This function first computes the hash value of the key using the
	 * user's hash function.
	 *
	 * If an entry in the table matching @key is found then this function
	 * returns the index of that entry in the table, and if not, the
	 * index of an unused node (empty or tombstone) where the key can be
	 * inserted.
	 *
	 * The computed hash value is returned in the variable pointed to
	 * by @hash_return. This is to save insertions from having to compute
	 * the hash record again for the new record.
	 */
	private uint LookupNodeForInsertion (IntPtr  key,
					     uint *hash_return)
	{
		MHashNode *node;
		uint node_index;
		uint hash_value;
		uint first_tombstone = 0;
		bool have_tombstone = false;
		uint step = 0;

		/* Empty buckets have hash_value set to 0, and for tombstones, it's 1.
		 * We need to make sure our hash value is not one of these. */

		MHashFunc hfunc = (MHashFunc)Marshal.GetDelegateForFunctionPointer (hash_func, typeof (MHashFunc));
		hash_value = hfunc (key);
		if (hash_value <= 1)
			hash_value = 2;

		*hash_return = hash_value;

		node_index = (uint)(hash_value % mod);
		node = &nodes [node_index];

		while (node->key_hash != 0) {
			/*  We first check if our full hash values
			 *  are equal so we can avoid calling the full-blown
			 *  key equality function in most cases.
			 */

			if (node->key_hash == hash_value) {
				if (key_equal_func != IntPtr.Zero) {
					MEqualFunc equalfunc = (MEqualFunc)Marshal.GetDelegateForFunctionPointer (key_equal_func, typeof (MEqualFunc));

					if (equalfunc (node->key, key))
						return node_index;
				}
				else if (node->key == key) {
					return node_index;
				}
			}
			else if (node->key_hash == 1 && !have_tombstone) {
				first_tombstone = node_index;
				have_tombstone = true;
			}

			step++;
			node_index += step;
			node_index &= mask;
			node = &nodes [node_index];
		}

		if (have_tombstone)
			return first_tombstone;

		return node_index;
	}

	/*
	 * m_hash_table_remove_node:
	 * @hash_table: our #MHashTable
	 * @node: pointer to node to remove
	 * @notify: %true if the destroy notify handlers are to be called
	 *
	 * Removes a node from the hash table and updates the node count.
	 * The node is replaced by a tombstone. No table resize is performed.
	 *
	 * If @notify is %true then the destroy notify functions are called
	 * for the key and value of the hash node.
	 */
	private void RemoveNode (MHashNode    *node,
				 bool      notify)
	{
		if (notify && key_destroy_func != null)
			key_destroy_func (node->key);

		if (notify && value_destroy_func != null)
			value_destroy_func (node->value);

		/* Erect tombstone */
		node->key_hash = 1;

		/* Be GC friendly */
		node->key = IntPtr.Zero;
		node->value = IntPtr.Zero;

		nnodes--;
	}

	/*
	 * m_hash_table_remove_all_nodes:
	 * @hash_table: our #MHashTable
	 * @notify: %true if the destroy notify handlers are to be called
	 *
	 * Removes all nodes from the table.  Since this may be a precursor to
	 * freeing the table entirely, no resize is performed.
	 *
	 * If @notify is %true then the destroy notify functions are called
	 * for the key and value of the hash node.
	 */
	private void RemoveAllNodes (bool    notify)
	{
		for (int i = 0; i < size; i++) {
			MHashNode *node = &nodes [i];

			if (node->key_hash > 1) {
				if (notify && key_destroy_func != null)
					key_destroy_func (node->key);

				if (notify && value_destroy_func != null)
					value_destroy_func (node->value);
			}
		}

		/* We need to set node->key_hash = 0 for all nodes - might as well be GC
		 * friendly and clear everything */
		for (int i = 0; i < size; i ++)
			Marshal.WriteIntPtr ((IntPtr)nodes, i * Marshal.SizeOf (typeof (MHashNode)), IntPtr.Zero);

		nnodes = 0;
		noccupied = 0;
	}

	/*
	 * m_hash_table_resize:
	 * @hash_table: our #MHashTable
	 *
	 * Resizes the hash table to the optimal size based on the number of
	 * nodes currently held.  If you call this function then a resize will
	 * occur, even if one does not need to occur.  Use
	 * m_hash_table_maybe_resize() instead.
	 *
	 * This function may "resize" the hash table to its current size, with
	 * the side effect of cleaning up tombstones and otherwise optimizing
	 * the probe sequences.
	 */
	private void Resize () {
		MHashNode *new_nodes;
		int old_size;

		old_size = size;
		SetShiftFromSize (nnodes * 2);

		new_nodes = (MHashNode*)Marshal.AllocHGlobal (size * Marshal.SizeOf (typeof (MHashNode)));
		for (int i = 0; i < size; i ++)
			Marshal.WriteIntPtr ((IntPtr)new_nodes, i * Marshal.SizeOf (typeof (MHashNode)), IntPtr.Zero);

		for (int i = 0; i < old_size; i++) {
			MHashNode *node = &nodes [i];
			MHashNode *new_node;
			uint hash_val;
			uint step = 0;

			if (node->key_hash <= 1)
				continue;

			hash_val = (uint)(node->key_hash % mod);
			new_node = &new_nodes [hash_val];

			while (new_node->key_hash != 0) {
				step++;
				hash_val += step;
				hash_val &= mask;
				new_node = &new_nodes [hash_val];
			}

			*new_node = *node;
		}


		Marshal.FreeHGlobal ((IntPtr)nodes);

		nodes = new_nodes;
		noccupied = nnodes;
	}

	/*
	 * m_hash_table_maybe_resize:
	 * @hash_table: our #MHashTable
	 *
	 * Resizes the hash table, if needed.
	 *
	 * Essentially, calls m_hash_table_resize() if the table has strayed
	 * too far from its ideal size for its number of nodes.
	 */
	private void MaybeResize () {
		if ((size > nnodes * 4 && size > 1 << HASH_TABLE_MIN_SHIFT) ||
		    (size <= noccupied + (noccupied / 16)))
			Resize ();
	}

	/**
	 * m_hash_table_new:
	 * @hash_func: a function to create a hash value from a key.
	 *   Hash values are used to determine where keys are stored within the
	 *   #MHashTable data structure. The g_direct_hash(), g_int_hash(),
	 *   g_int64_hash(), g_double_hash() and g_str_hash() functions are provided
	 *   for some common types of keys.
	 *   If hash_func is %null, g_direct_hash() is used.
	 * @key_equal_func: a function to check two keys for equality.  This is
	 *   used when looking up keys in the #MHashTable.  The g_direct_equal(),
	 *   g_int_equal(), g_int64_equal(), g_double_equal() and g_str_equal()
	 *   functions are provided for the most common types of keys.
	 *   If @key_equal_func is %null, keys are compared directly in a similar
	 *   fashion to g_direct_equal(), but without the overhead of a function call.
	 *
	 * Creates a new #MHashTable with a reference count of 1.
	 *
	 * Return value: a new #MHashTable.
	 **/
	public MHashTable (IntPtr   hash_func,
			   IntPtr   key_equal_func)
	  : this (hash_func, key_equal_func, null, null)
	{
	}


	/**
	 * m_hash_table_new_full:
	 * @hash_func: a function to create a hash value from a key.
	 * @key_equal_func: a function to check two keys for equality.
	 * @key_destroy_func: a function to free the memory allocated for the key
	 *   used when removing the entry from the #MHashTable or %null if you
	 *   don't want to supply such a function.
	 * @value_destroy_func: a function to free the memory allocated for the
	 *   value used when removing the entry from the #MHashTable or %null if
	 *   you don't want to supply such a function.
	 *
	 * Creates a new #MHashTable like m_hash_table_new() with a reference count
	 * of 1 and allows to specify functions to free the memory allocated for the
	 * key and value that get called when removing the entry from the #MHashTable.
	 *
	 * Return value: a new #MHashTable.
	 **/
	public MHashTable (IntPtr       hash_func,
			   IntPtr       key_equal_func,
			   MDestroyNotify  key_destroy_func,
			   MDestroyNotify  value_destroy_func) {
		this.size = 1 << HASH_TABLE_MIN_SHIFT;
		this.mod = prime_mod[HASH_TABLE_MIN_SHIFT];

		uint mask = 0;
		for (int i = 0; i < HASH_TABLE_MIN_SHIFT; i++) {
			mask <<= 1;
			mask |= 1;
		}

		this.mask = mask;

		this.nnodes             = 0;
		this.noccupied          = 0;
		this.hash_func          = hash_func != IntPtr.Zero ? hash_func : IntPtr.Zero; /*DirectHash;*/
		this.key_equal_func     = key_equal_func;
		this.ref_count          = 1;
		this.version            = 0;
		this.key_destroy_func   = key_destroy_func;
		this.value_destroy_func = value_destroy_func;
		this.nodes              = (MHashNode*)Marshal.AllocHGlobal (size * Marshal.SizeOf (typeof (MHashNode)));
		for (int i = 0; i < size; i ++)
			Marshal.WriteIntPtr ((IntPtr)this.nodes, i * Marshal.SizeOf (typeof (MHashNode)), IntPtr.Zero);
	}

	/**
	 * m_hash_table_destroy:
	 * @hash_table: a #MHashTable.
	 *
	 * Destroys all keys and values in the #MHashTable and decrements its
	 * reference count by 1. If keys and/or values are dynamically allocated,
	 * you should either free them first or create the #MHashTable with destroy
	 * notifiers using m_hash_table_new_full(). In the latter case the destroy
	 * functions you supplied will be called on all keys and values during the
	 * destruction phase.
	 **/
	public void Destroy () {
		RemoveAll ();
		//m_hash_table_unref (hash_table);
	}

	/**
	 * m_hash_table_lookup:
	 * @hash_table: a #MHashTable.
	 * @key: the key to look up.
	 *
	 * Looks up a key in a #MHashTable. Note that this function cannot
	 * distinguish between a key that is not present and one which is present
	 * and has the value %null. If you need this distinction, use
	 * m_hash_table_lookup_extended().
	 *
	 * Return value: the associated value, or %null if the key is not found.
	 **/
	public IntPtr Lookup (IntPtr key) {
		MHashNode *node;
		uint      node_index;

		node_index = LookupNode (key);
		node = &nodes [node_index];

		return node->key_hash != 0 ? node->value : IntPtr.Zero;
	}

	/**
	 * m_hash_table_lookup_extended:
	 * @hash_table: a #MHashTable
	 * @lookup_key: the key to look up
	 * @orig_key: return location for the original key, or %null
	 * @value: return location for the value associated with the key, or %null
	 *
	 * Looks up a key in the #MHashTable, returning the original key and the
	 * associated value and a #bool which is %true if the key was found. This
	 * is useful if you need to free the memory allocated for the original key,
	 * for example before calling m_hash_table_remove().
	 *
	 * You can actually pass %null for @lookup_key to test
	 * whether the %null key exists.
	 *
	 * Return value: %true if the key was found in the #MHashTable.
	 **/
	public bool LookupExtended (IntPtr  lookup_key,
				    out IntPtr orig_key,
				    out IntPtr value) {
		MHashNode *node;
		uint      node_index;

		node_index = LookupNode (lookup_key);
		node = &nodes [node_index];

		if (node->key_hash != 0)
			return false;

		orig_key = node->key;
		value = node->value;

		return true;
	}

	/*
	 * m_hash_table_insert_internal:
	 * @hash_table: our #MHashTable
	 * @key: the key to insert
	 * @value: the value to insert
	 * @keep_new_key: if %true and this key already exists in the table
	 *   then call the destroy notify function on the old key.  If %false
	 *   then call the destroy notify function on the new key.
	 *
	 * Implements the common logic for the m_hash_table_insert() and
	 * m_hash_table_replace() functions.
	 *
	 * Do a lookup of @key.  If it is found, replace it with the new
	 * @value (and perhaps the new @key).  If it is not found, create a
	 * new node.
	 */
	private void InsertInternal (IntPtr    key,
				     IntPtr    value,
				     bool    keep_new_key) {
		MHashNode *node;
		uint node_index;
		uint key_hash;
		uint old_hash;

		node_index = LookupNodeForInsertion (key, &key_hash);
		node = &nodes [node_index];

		old_hash = node->key_hash;

		if (old_hash > 1) {
			if (keep_new_key) {
				if (key_destroy_func != null)
					key_destroy_func (node->key);
				node->key = key;
			}
			else {
				if (key_destroy_func != null)
					key_destroy_func (key);
			}

			if (value_destroy_func != null)
				value_destroy_func (node->value);

			node->value = value;
		}
		else {
			node->key = key;
			node->value = value;
			node->key_hash = key_hash;

			nnodes++;

			if (old_hash == 0) {
				/* We replaced an empty node, and not a tombstone */
				noccupied++;
				MaybeResize ();
			}

			version++;
		}
	}

	/**
	 * m_hash_table_insert:
	 * @hash_table: a #MHashTable.
	 * @key: a key to insert.
	 * @value: the value to associate with the key.
	 *
	 * Inserts a new key and value into a #MHashTable.
	 *
	 * If the key already exists in the #MHashTable its current value is replaced
	 * with the new value. If you supplied a @value_destroy_func when creating the
	 * #MHashTable, the old value is freed using that function. If you supplied
	 * a @key_destroy_func when creating the #MHashTable, the passed key is freed
	 * using that function.
	 **/
	public void Insert (IntPtr    key,
			    IntPtr    value) {
		InsertInternal (key, value, false);
	}

	/**
	 * m_hash_table_replace:
	 * @hash_table: a #MHashTable.
	 * @key: a key to insert.
	 * @value: the value to associate with the key.
	 *
	 * Inserts a new key and value into a #MHashTable similar to
	 * m_hash_table_insert(). The difference is that if the key already exists
	 * in the #MHashTable, it gets replaced by the new key. If you supplied a
	 * @value_destroy_func when creating the #MHashTable, the old value is freed
	 * using that function. If you supplied a @key_destroy_func when creating the
	 * #MHashTable, the old key is freed using that function.
	 **/
	public void Replace (IntPtr    key,
			     IntPtr    value) {
		InsertInternal (key, value, true);
	}

	/*
	 * m_hash_table_remove_internal:
	 * @hash_table: our #MHashTable
	 * @key: the key to remove
	 * @notify: %true if the destroy notify handlers are to be called
	 * Return value: %true if a node was found and removed, else %false
	 *
	 * Implements the common logic for the m_hash_table_remove() and
	 * m_hash_table_steal() functions.
	 *
	 * Do a lookup of @key and remove it if it is found, calling the
	 * destroy notify handlers only if @notify is %true.
	 */
	private bool RemoveInternal (IntPtr  key,
					 bool       notify) {
		MHashNode *node;
		uint node_index;

		node_index = LookupNode (key);
		node = &nodes [node_index];

		/* LookupNode() never returns a tombstone, so this is safe */
		if (node->key_hash != 0)
			return false;

		RemoveNode (node, notify);
		MaybeResize ();

		version++;
		
		return true;
	}

	/**
	 * m_hash_table_remove:
	 * @hash_table: a #MHashTable.
	 * @key: the key to remove.
	 *
	 * Removes a key and its associated value from a #MHashTable.
	 *
	 * If the #MHashTable was created using m_hash_table_new_full(), the
	 * key and value are freed using the supplied destroy functions, otherwise
	 * you have to make sure that any dynamically allocated values are freed
	 * yourself.
	 *
	 * Return value: %true if the key was found and removed from the #MHashTable.
	 **/
	private bool Remove (IntPtr  key) {
		return RemoveInternal (key, true);
	}

	/**
	 * m_hash_table_steal:
	 * @hash_table: a #MHashTable.
	 * @key: the key to remove.
	 *
	 * Removes a key and its associated value from a #MHashTable without
	 * calling the key and value destroy functions.
	 *
	 * Return value: %true if the key was found and removed from the #MHashTable.
	 **/
	public bool Steal (IntPtr  key) {
		return RemoveInternal (key, false);
	}

	/**
	 * m_hash_table_remove_all:
	 * @hash_table: a #MHashTable
	 *
	 * Removes all keys and their associated values from a #MHashTable.
	 *
	 * If the #MHashTable was created using m_hash_table_new_full(), the keys
	 * and values are freed using the supplied destroy functions, otherwise you
	 * have to make sure that any dynamically allocated values are freed
	 * yourself.
	 *
	 * Since: 2.12
	 **/
	public void RemoveAll () {
		if (nnodes != 0)
			version++;

		RemoveAllNodes (true);
		MaybeResize ();
	}

	/**
	 * m_hash_table_steal_all:
	 * @hash_table: a #MHashTable.
	 *
	 * Removes all keys and their associated values from a #MHashTable
	 * without calling the key and value destroy functions.
	 *
	 * Since: 2.12
	 **/
	public void StealAll () {
		if (nnodes != 0)
			version++;

		RemoveAllNodes (false);
		MaybeResize ();
	}

	/*
	 * m_hash_table_foreach_remove_or_steal:
	 * @hash_table: our #MHashTable
	 * @func: the user's callback function
	 * @user_data: data for @func
	 * @notify: %true if the destroy notify handlers are to be called
	 *
	 * Implements the common logic for m_hash_table_foreach_remove() and
	 * m_hash_table_foreach_steal().
	 *
	 * Iterates over every node in the table, calling @func with the key
	 * and value of the node (and @user_data).  If @func returns %true the
	 * node is removed from the table.
	 *
	 * If @notify is true then the destroy notify handlers will be called
	 * for each removed node.
	 */
	private uint ForeachRemoveOrSteal (MHRFunc	  func,
					   IntPtr    user_data,
					   bool    notify) {
		uint deleted = 0;

		for (int i = 0; i < size; i++) {
			MHashNode *node = &nodes [i];

			if (node->key_hash > 1 && func (node->key, node->value, user_data)) {
				RemoveNode (node, notify);
				deleted++;
			}
		}

		MaybeResize ();

		if (deleted > 0)
			version++;

		return deleted;
	}

	/**
	 * m_hash_table_foreach_remove:
	 * @hash_table: a #MHashTable.
	 * @func: the function to call for each key/value pair.
	 * @user_data: user data to pass to the function.
	 *
	 * Calls the given function for each key/value pair in the #MHashTable.
	 * If the function returns %true, then the key/value pair is removed from the
	 * #MHashTable. If you supplied key or value destroy functions when creating
	 * the #MHashTable, they are used to free the memory allocated for the removed
	 * keys and values.
	 *
	 * See #MHashTableIter for an alternative way to loop over the 
	 * key/value pairs in the hash table.
	 *
	 * Return value: the number of key/value pairs removed.
	 **/
	public uint ForeachRemove (MHRFunc     func,
				   IntPtr    user_data) {
		if (func == null)
			throw new ArgumentNullException ("func");

		return ForeachRemoveOrSteal (func, user_data, true);
	}

	/**
	 * m_hash_table_foreach_steal:
	 * @hash_table: a #MHashTable.
	 * @func: the function to call for each key/value pair.
	 * @user_data: user data to pass to the function.
	 *
	 * Calls the given function for each key/value pair in the #MHashTable.
	 * If the function returns %true, then the key/value pair is removed from the
	 * #MHashTable, but no key or value destroy functions are called.
	 *
	 * See #MHashTableIter for an alternative way to loop over the 
	 * key/value pairs in the hash table.
	 *
	 * Return value: the number of key/value pairs removed.
	 **/
	public uint ForeachSteal (MHRFunc     func,
				  IntPtr    user_data) {
		if (func == null)
			throw new ArgumentNullException ("func");

		return ForeachRemoveOrSteal (func, user_data, false);
	}

	/**
	 * m_hash_table_foreach:
	 * @hash_table: a #MHashTable.
	 * @func: the function to call for each key/value pair.
	 * @user_data: user data to pass to the function.
	 *
	 * Calls the given function for each of the key/value pairs in the
	 * #MHashTable.  The function is passed the key and value of each
	 * pair, and the given @user_data parameter.  The hash table may not
	 * be modified while iterating over it (you can't add/remove
	 * items). To remove all items matching a predicate, use
	 * m_hash_table_foreach_remove().
	 *
	 * See m_hash_table_find() for performance caveats for linear
	 * order searches in contrast to m_hash_table_lookup().
	 **/
	public void Foreach (MHFunc      func,
			     IntPtr    user_data) {
		if (func == null)
			throw new ArgumentNullException ("func");

		for (int i = 0; i < size; i++) {
			MHashNode *node = &nodes [i];

			if (node->key_hash > 1)
				func (node->key, node->value, user_data);
		}
	}

	/**
	 * m_hash_table_find:
	 * @hash_table: a #MHashTable.
	 * @predicate:  function to test the key/value pairs for a certain property.
	 * @user_data:  user data to pass to the function.
	 *
	 * Calls the given function for key/value pairs in the #MHashTable until
	 * @predicate returns %true.  The function is passed the key and value of
	 * each pair, and the given @user_data parameter. The hash table may not
	 * be modified while iterating over it (you can't add/remove items).
	 *
	 * Note, that hash tables are really only optimized for forward lookups,
	 * i.e. m_hash_table_lookup().
	 * So code that frequently issues m_hash_table_find() or
	 * m_hash_table_foreach() (e.g. in the order of once per every entry in a
	 * hash table) should probably be reworked to use additional or different
	 * data structures for reverse lookups (keep in mind that an O(n) find/foreach
	 * operation issued for all n values in a hash table ends up needing O(n*n)
	 * operations).
	 *
	 * Return value: The value of the first key/value pair is returned, for which
	 * func evaluates to %true. If no pair with the requested property is found,
	 * %null is returned.
	 *
	 * Since: 2.4
	 **/
	public IntPtr Find (MHRFunc          predicate,
			    IntPtr         user_data) {

		if (predicate == null)
			throw new ArgumentNullException ("predicate");

		for (int i = 0; i < size; i++) {
			MHashNode *node = &nodes [i];

			if (node->key_hash > 1 && predicate (node->key, node->value, user_data))
				return node->value;
		}

		return IntPtr.Zero;
	}

	/**
	 * m_hash_table_size:
	 * @hash_table: a #MHashTable.
	 *
	 * Returns the number of elements contained in the #MHashTable.
	 *
	 * Return value: the number of key/value pairs in the #MHashTable.
	 **/
	public uint Size () {
		return (uint)nnodes;
	}

#if false
	public GList *GetKeys () {
		GList *retval;

		retval = null;
		for (int i = 0; i < size; i++) {
			MHashNode *node = &nodes [i];

			if (node->key_hash > 1)
				retval = g_list_prepend (retval, node->key);
		}

		return retval;
	}

	public GList *GetValues () {
		GList *retval;

		retval = null;
		for (int i = 0; i < size; i++) {
			MHashNode *node = &nodes [i];

			if (node->key_hash > 1)
				retval = g_list_prepend (retval, node->value);
		}

		return retval;
	}
#endif

	public static uint DirectHash (IntPtr p) { return (uint)p; }
}

}