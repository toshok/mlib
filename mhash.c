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

/*
 * MT safe
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>  /* memset */

#include "mhash.h"
//#include "galias.h"

#define HASH_TABLE_MIN_SHIFT 3  /* 1 << 3 == 8 buckets */

typedef struct _MHashNode      MHashNode;

struct _MHashNode
{
  gpointer   key;
  gpointer   value;

  /* If key_hash == 0, node is not in use
   * If key_hash == 1, node is a tombstone
   * If key_hash >= 2, node contains data */
  guint      key_hash;
};

struct _MHashTable
{
  gint             size;
  gint             mod;
  guint            mask;
  gint             nnodes;
  gint             noccupied;  /* nnodes + tombstones */
  MHashNode       *nodes;
  MHashFunc        hash_func;
  GEqualFunc       key_equal_func;
  volatile gint    ref_count;
  /*
   * Tracks the structure of the hash table, not its contents: is only
   * incremented when a node is added or removed (is not incremented
   * when the key or data of a node is modified).
   */
  int              version;
  GDestroyNotify   key_destroy_func;
  GDestroyNotify   value_destroy_func;
};

typedef struct
{
  MHashTable  *hash_table;
  gpointer     dummy1;
  gpointer     dummy2;
  int          position;
  gboolean     dummy3;
  int          version;
} RealIter;

/* Each table size has an associated prime modulo (the first prime
 * lower than the table size) used to find the initial bucket. Probing
 * then works modulo 2^n. The prime modulo is necessary to get a
 * good distribution with poor hash functions. */
static const gint prime_mod [] =
{
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

static void
m_hash_table_set_shift (MHashTable *hash_table, gint shift)
{
  gint i;
  guint mask = 0;

  hash_table->size = 1 << shift;
  hash_table->mod  = prime_mod [shift];

  for (i = 0; i < shift; i++)
    {
      mask <<= 1;
      mask |= 1;
    }

  hash_table->mask = mask;
}

static gint
m_hash_table_find_closest_shift (gint n)
{
  gint i;

  for (i = 0; n; i++)
    n >>= 1;

  return i;
}

static void
m_hash_table_set_shift_from_size (MHashTable *hash_table, gint size)
{
  gint shift;

  shift = m_hash_table_find_closest_shift (size);
  shift = MAX (shift, HASH_TABLE_MIN_SHIFT);

  m_hash_table_set_shift (hash_table, shift);
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
static inline guint
m_hash_table_lookup_node (MHashTable    *hash_table,
                          gconstpointer  key)
{
  MHashNode *node;
  guint node_index;
  guint hash_value;
  guint step = 0;

  /* Empty buckets have hash_value set to 0, and for tombstones, it's 1.
   * We need to make sure our hash value is not one of these. */

  hash_value = (* hash_table->hash_func) (key);
  if (G_UNLIKELY (hash_value <= 1))
    hash_value = 2;

  node_index = hash_value % hash_table->mod;
  node = &hash_table->nodes [node_index];

  while (node->key_hash)
    {
      /*  We first check if our full hash values
       *  are equal so we can avoid calling the full-blown
       *  key equality function in most cases.
       */

      if (node->key_hash == hash_value)
        {
          if (hash_table->key_equal_func)
            {
              if (hash_table->key_equal_func (node->key, key))
                break;
            }
          else if (node->key == key)
            {
              break;
            }
        }

      step++;
      node_index += step;
      node_index &= hash_table->mask;
      node = &hash_table->nodes [node_index];
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
static inline guint
m_hash_table_lookup_node_for_insertion (MHashTable    *hash_table,
                                        gconstpointer  key,
                                        guint         *hash_return)
{
  MHashNode *node;
  guint node_index;
  guint hash_value;
  guint first_tombstone;
  gboolean have_tombstone = FALSE;
  guint step = 0;

  /* Empty buckets have hash_value set to 0, and for tombstones, it's 1.
   * We need to make sure our hash value is not one of these. */

  hash_value = (* hash_table->hash_func) (key);
  if (G_UNLIKELY (hash_value <= 1))
    hash_value = 2;

  *hash_return = hash_value;

  node_index = hash_value % hash_table->mod;
  node = &hash_table->nodes [node_index];

  while (node->key_hash)
    {
      /*  We first check if our full hash values
       *  are equal so we can avoid calling the full-blown
       *  key equality function in most cases.
       */

      if (node->key_hash == hash_value)
        {
          if (hash_table->key_equal_func)
            {
              if (hash_table->key_equal_func (node->key, key))
                return node_index;
            }
          else if (node->key == key)
            {
              return node_index;
            }
        }
      else if (node->key_hash == 1 && !have_tombstone)
        {
          first_tombstone = node_index;
          have_tombstone = TRUE;
        }

      step++;
      node_index += step;
      node_index &= hash_table->mask;
      node = &hash_table->nodes [node_index];
    }

  if (have_tombstone)
    return first_tombstone;

  return node_index;
}

/*
 * m_hash_table_remove_node:
 * @hash_table: our #MHashTable
 * @node: pointer to node to remove
 * @notify: %TRUE if the destroy notify handlers are to be called
 *
 * Removes a node from the hash table and updates the node count.
 * The node is replaced by a tombstone. No table resize is performed.
 *
 * If @notify is %TRUE then the destroy notify functions are called
 * for the key and value of the hash node.
 */
static void
m_hash_table_remove_node (MHashTable   *hash_table,
                          MHashNode    *node,
                          gboolean      notify)
{
  if (notify && hash_table->key_destroy_func)
    hash_table->key_destroy_func (node->key);

  if (notify && hash_table->value_destroy_func)
    hash_table->value_destroy_func (node->value);

  /* Erect tombstone */
  node->key_hash = 1;

  /* Be GC friendly */
  node->key = NULL;
  node->value = NULL;

  hash_table->nnodes--;
}

/*
 * m_hash_table_remove_all_nodes:
 * @hash_table: our #MHashTable
 * @notify: %TRUE if the destroy notify handlers are to be called
 *
 * Removes all nodes from the table.  Since this may be a precursor to
 * freeing the table entirely, no resize is performed.
 *
 * If @notify is %TRUE then the destroy notify functions are called
 * for the key and value of the hash node.
 */
static void
m_hash_table_remove_all_nodes (MHashTable *hash_table,
                               gboolean    notify)
{
  int i;

  for (i = 0; i < hash_table->size; i++)
    {
      MHashNode *node = &hash_table->nodes [i];

      if (node->key_hash > 1)
        {
          if (notify && hash_table->key_destroy_func)
            hash_table->key_destroy_func (node->key);

          if (notify && hash_table->value_destroy_func)
            hash_table->value_destroy_func (node->value);
        }
    }

  /* We need to set node->key_hash = 0 for all nodes - might as well be GC
   * friendly and clear everything */
  memset (hash_table->nodes, 0, hash_table->size * sizeof (MHashNode));

  hash_table->nnodes = 0;
  hash_table->noccupied = 0;
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
static void
m_hash_table_resize (MHashTable *hash_table)
{
  MHashNode *new_nodes;
  gint old_size;
  gint i;

  old_size = hash_table->size;
  m_hash_table_set_shift_from_size (hash_table, hash_table->nnodes * 2);

  new_nodes = g_new0 (MHashNode, hash_table->size);

  for (i = 0; i < old_size; i++)
    {
      MHashNode *node = &hash_table->nodes [i];
      MHashNode *new_node;
      guint hash_val;
      guint step = 0;

      if (node->key_hash <= 1)
        continue;

      hash_val = node->key_hash % hash_table->mod;
      new_node = &new_nodes [hash_val];

      while (new_node->key_hash)
        {
          step++;
          hash_val += step;
          hash_val &= hash_table->mask;
          new_node = &new_nodes [hash_val];
        }

      *new_node = *node;
    }

  g_free (hash_table->nodes);
  hash_table->nodes = new_nodes;
  hash_table->noccupied = hash_table->nnodes;
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
static inline void
m_hash_table_maybe_resize (MHashTable *hash_table)
{
  gint noccupied = hash_table->noccupied;
  gint size = hash_table->size;

  if ((size > hash_table->nnodes * 4 && size > 1 << HASH_TABLE_MIN_SHIFT) ||
      (size <= noccupied + (noccupied / 16)))
    m_hash_table_resize (hash_table);
}

/**
 * m_hash_table_new:
 * @hash_func: a function to create a hash value from a key.
 *   Hash values are used to determine where keys are stored within the
 *   #MHashTable data structure. The g_direct_hash(), g_int_hash(),
 *   g_int64_hash(), g_double_hash() and g_str_hash() functions are provided
 *   for some common types of keys.
 *   If hash_func is %NULL, g_direct_hash() is used.
 * @key_equal_func: a function to check two keys for equality.  This is
 *   used when looking up keys in the #MHashTable.  The g_direct_equal(),
 *   g_int_equal(), g_int64_equal(), g_double_equal() and g_str_equal()
 *   functions are provided for the most common types of keys.
 *   If @key_equal_func is %NULL, keys are compared directly in a similar
 *   fashion to g_direct_equal(), but without the overhead of a function call.
 *
 * Creates a new #MHashTable with a reference count of 1.
 *
 * Return value: a new #MHashTable.
 **/
MHashTable*
m_hash_table_new (MHashFunc    hash_func,
                  GEqualFunc   key_equal_func)
{
  return m_hash_table_new_full (hash_func, key_equal_func, NULL, NULL);
}


/**
 * m_hash_table_new_full:
 * @hash_func: a function to create a hash value from a key.
 * @key_equal_func: a function to check two keys for equality.
 * @key_destroy_func: a function to free the memory allocated for the key
 *   used when removing the entry from the #MHashTable or %NULL if you
 *   don't want to supply such a function.
 * @value_destroy_func: a function to free the memory allocated for the
 *   value used when removing the entry from the #MHashTable or %NULL if
 *   you don't want to supply such a function.
 *
 * Creates a new #MHashTable like m_hash_table_new() with a reference count
 * of 1 and allows to specify functions to free the memory allocated for the
 * key and value that get called when removing the entry from the #MHashTable.
 *
 * Return value: a new #MHashTable.
 **/
MHashTable*
m_hash_table_new_full (MHashFunc       hash_func,
                       GEqualFunc      key_equal_func,
                       GDestroyNotify  key_destroy_func,
                       GDestroyNotify  value_destroy_func)
{
  MHashTable *hash_table;

  hash_table = g_slice_new (MHashTable);
  m_hash_table_set_shift (hash_table, HASH_TABLE_MIN_SHIFT);
  hash_table->nnodes             = 0;
  hash_table->noccupied          = 0;
  hash_table->hash_func          = hash_func ? hash_func : g_direct_hash;
  hash_table->key_equal_func     = key_equal_func;
  hash_table->ref_count          = 1;
  hash_table->version            = 0;
  hash_table->key_destroy_func   = key_destroy_func;
  hash_table->value_destroy_func = value_destroy_func;
  hash_table->nodes              = g_new0 (MHashNode, hash_table->size);

  return hash_table;
}

/**
 * m_hash_table_iter_init:
 * @iter: an uninitialized #MHashTableIter.
 * @hash_table: a #MHashTable.
 *
 * Initializes a key/value pair iterator and associates it with
 * @hash_table. Modifying the hash table after calling this function
 * invalidates the returned iterator.
 * |[
 * MHashTableIter iter;
 * gpointer key, value;
 *
 * m_hash_table_iter_init (&iter, hash_table);
 * while (m_hash_table_iter_next (&iter, &key, &value)) 
 *   {
 *     /&ast; do something with key and value &ast;/
 *   }
 * ]|
 *
 * Since: 2.16
 **/
void
m_hash_table_iter_init (MHashTableIter *iter,
			MHashTable     *hash_table)
{
  RealIter *ri = (RealIter *) iter;

  g_return_if_fail (iter != NULL);
  g_return_if_fail (hash_table != NULL);

  ri->hash_table = hash_table;
  ri->position = -1;
  ri->version = hash_table->version;
}

/**
 * m_hash_table_iter_next:
 * @iter: an initialized #MHashTableIter.
 * @key: a location to store the key, or %NULL.
 * @value: a location to store the value, or %NULL.
 *
 * Advances @iter and retrieves the key and/or value that are now
 * pointed to as a result of this advancement. If %FALSE is returned,
 * @key and @value are not set, and the iterator becomes invalid.
 *
 * Return value: %FALSE if the end of the #MHashTable has been reached.
 *
 * Since: 2.16
 **/
gboolean
m_hash_table_iter_next (MHashTableIter *iter,
			gpointer       *key,
			gpointer       *value)
{
  RealIter *ri = (RealIter *) iter;
  MHashNode *node;
  gint position;

  g_return_val_if_fail (iter != NULL, FALSE);
  g_return_val_if_fail (ri->version == ri->hash_table->version, FALSE);
  g_return_val_if_fail (ri->position < ri->hash_table->size, FALSE);

  position = ri->position;

  do
    {
      position++;
      if (position >= ri->hash_table->size)
        {
          ri->position = position;
          return FALSE;
        }

      node = &ri->hash_table->nodes [position];
    }
  while (node->key_hash <= 1);

  if (key != NULL)
    *key = node->key;
  if (value != NULL)
    *value = node->value;

  ri->position = position;
  return TRUE;
}

/**
 * m_hash_table_iter_get_hash_table:
 * @iter: an initialized #MHashTableIter.
 *
 * Returns the #MHashTable associated with @iter.
 *
 * Return value: the #MHashTable associated with @iter.
 *
 * Since: 2.16
 **/
MHashTable *
m_hash_table_iter_get_hash_table (MHashTableIter *iter)
{
  g_return_val_if_fail (iter != NULL, NULL);

  return ((RealIter *) iter)->hash_table;
}

static void
iter_remove_or_steal (RealIter *ri, gboolean notify)
{
  g_return_if_fail (ri != NULL);
  g_return_if_fail (ri->version == ri->hash_table->version);
  g_return_if_fail (ri->position >= 0);
  g_return_if_fail (ri->position < ri->hash_table->size);

  m_hash_table_remove_node (ri->hash_table, &ri->hash_table->nodes [ri->position], notify);

  ri->version++;
  ri->hash_table->version++;
}

/**
 * m_hash_table_iter_remove():
 * @iter: an initialized #MHashTableIter.
 *
 * Removes the key/value pair currently pointed to by the iterator
 * from its associated #MHashTable. Can only be called after
 * m_hash_table_iter_next() returned %TRUE, and cannot be called more
 * than once for the same key/value pair.
 *
 * If the #MHashTable was created using m_hash_table_new_full(), the
 * key and value are freed using the supplied destroy functions, otherwise
 * you have to make sure that any dynamically allocated values are freed 
 * yourself.
 *
 * Since: 2.16
 **/
void
m_hash_table_iter_remove (MHashTableIter *iter)
{
  iter_remove_or_steal ((RealIter *) iter, TRUE);
}

/**
 * m_hash_table_iter_steal():
 * @iter: an initialized #MHashTableIter.
 *
 * Removes the key/value pair currently pointed to by the iterator
 * from its associated #MHashTable, without calling the key and value
 * destroy functions. Can only be called after
 * m_hash_table_iter_next() returned %TRUE, and cannot be called more
 * than once for the same key/value pair.
 *
 * Since: 2.16
 **/
void
m_hash_table_iter_steal (MHashTableIter *iter)
{
  iter_remove_or_steal ((RealIter *) iter, FALSE);
}


/**
 * m_hash_table_ref:
 * @hash_table: a valid #MHashTable.
 *
 * Atomically increments the reference count of @hash_table by one.
 * This function is MT-safe and may be called from any thread.
 *
 * Return value: the passed in #MHashTable.
 *
 * Since: 2.10
 **/
MHashTable*
m_hash_table_ref (MHashTable *hash_table)
{
  g_return_val_if_fail (hash_table != NULL, NULL);
  g_return_val_if_fail (hash_table->ref_count > 0, hash_table);

  g_atomic_int_add (&hash_table->ref_count, 1);
  return hash_table;
}

/**
 * m_hash_table_unref:
 * @hash_table: a valid #MHashTable.
 *
 * Atomically decrements the reference count of @hash_table by one.
 * If the reference count drops to 0, all keys and values will be
 * destroyed, and all memory allocated by the hash table is released.
 * This function is MT-safe and may be called from any thread.
 *
 * Since: 2.10
 **/
void
m_hash_table_unref (MHashTable *hash_table)
{
  g_return_if_fail (hash_table != NULL);
  g_return_if_fail (hash_table->ref_count > 0);

  if (g_atomic_int_exchange_and_add (&hash_table->ref_count, -1) - 1 == 0)
    {
      m_hash_table_remove_all_nodes (hash_table, TRUE);
      g_free (hash_table->nodes);
      g_slice_free (MHashTable, hash_table);
    }
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
void
m_hash_table_destroy (MHashTable *hash_table)
{
  g_return_if_fail (hash_table != NULL);
  g_return_if_fail (hash_table->ref_count > 0);

  m_hash_table_remove_all (hash_table);
  m_hash_table_unref (hash_table);
}

/**
 * m_hash_table_lookup:
 * @hash_table: a #MHashTable.
 * @key: the key to look up.
 *
 * Looks up a key in a #MHashTable. Note that this function cannot
 * distinguish between a key that is not present and one which is present
 * and has the value %NULL. If you need this distinction, use
 * m_hash_table_lookup_extended().
 *
 * Return value: the associated value, or %NULL if the key is not found.
 **/
gpointer
m_hash_table_lookup (MHashTable   *hash_table,
                     gconstpointer key)
{
  MHashNode *node;
  guint      node_index;

  g_return_val_if_fail (hash_table != NULL, NULL);

  node_index = m_hash_table_lookup_node (hash_table, key);
  node = &hash_table->nodes [node_index];

  return node->key_hash ? node->value : NULL;
}

/**
 * m_hash_table_lookup_extended:
 * @hash_table: a #MHashTable
 * @lookup_key: the key to look up
 * @orig_key: return location for the original key, or %NULL
 * @value: return location for the value associated with the key, or %NULL
 *
 * Looks up a key in the #MHashTable, returning the original key and the
 * associated value and a #gboolean which is %TRUE if the key was found. This
 * is useful if you need to free the memory allocated for the original key,
 * for example before calling m_hash_table_remove().
 *
 * You can actually pass %NULL for @lookup_key to test
 * whether the %NULL key exists.
 *
 * Return value: %TRUE if the key was found in the #MHashTable.
 **/
gboolean
m_hash_table_lookup_extended (MHashTable    *hash_table,
                              gconstpointer  lookup_key,
                              gpointer      *orig_key,
                              gpointer      *value)
{
  MHashNode *node;
  guint      node_index;

  g_return_val_if_fail (hash_table != NULL, FALSE);

  node_index = m_hash_table_lookup_node (hash_table, lookup_key);
  node = &hash_table->nodes [node_index];

  if (!node->key_hash)
    return FALSE;

  if (orig_key)
    *orig_key = node->key;

  if (value)
    *value = node->value;

  return TRUE;
}

/*
 * m_hash_table_insert_internal:
 * @hash_table: our #MHashTable
 * @key: the key to insert
 * @value: the value to insert
 * @keep_new_key: if %TRUE and this key already exists in the table
 *   then call the destroy notify function on the old key.  If %FALSE
 *   then call the destroy notify function on the new key.
 *
 * Implements the common logic for the m_hash_table_insert() and
 * m_hash_table_replace() functions.
 *
 * Do a lookup of @key.  If it is found, replace it with the new
 * @value (and perhaps the new @key).  If it is not found, create a
 * new node.
 */
static void
m_hash_table_insert_internal (MHashTable *hash_table,
                              gpointer    key,
                              gpointer    value,
                              gboolean    keep_new_key)
{
  MHashNode *node;
  guint node_index;
  guint key_hash;
  guint old_hash;

  g_return_if_fail (hash_table != NULL);
  g_return_if_fail (hash_table->ref_count > 0);

  node_index = m_hash_table_lookup_node_for_insertion (hash_table, key, &key_hash);
  node = &hash_table->nodes [node_index];

  old_hash = node->key_hash;

  if (old_hash > 1)
    {
      if (keep_new_key)
        {
          if (hash_table->key_destroy_func)
            hash_table->key_destroy_func (node->key);
          node->key = key;
        }
      else
        {
          if (hash_table->key_destroy_func)
            hash_table->key_destroy_func (key);
        }

      if (hash_table->value_destroy_func)
        hash_table->value_destroy_func (node->value);

      node->value = value;
    }
  else
    {
      node->key = key;
      node->value = value;
      node->key_hash = key_hash;

      hash_table->nnodes++;

      if (old_hash == 0)
        {
          /* We replaced an empty node, and not a tombstone */
          hash_table->noccupied++;
          m_hash_table_maybe_resize (hash_table);
        }

      hash_table->version++;
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
void
m_hash_table_insert (MHashTable *hash_table,
                     gpointer    key,
                     gpointer    value)
{
  m_hash_table_insert_internal (hash_table, key, value, FALSE);
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
void
m_hash_table_replace (MHashTable *hash_table,
                      gpointer    key,
                      gpointer    value)
{
  m_hash_table_insert_internal (hash_table, key, value, TRUE);
}

/*
 * m_hash_table_remove_internal:
 * @hash_table: our #MHashTable
 * @key: the key to remove
 * @notify: %TRUE if the destroy notify handlers are to be called
 * Return value: %TRUE if a node was found and removed, else %FALSE
 *
 * Implements the common logic for the m_hash_table_remove() and
 * m_hash_table_steal() functions.
 *
 * Do a lookup of @key and remove it if it is found, calling the
 * destroy notify handlers only if @notify is %TRUE.
 */
static gboolean
m_hash_table_remove_internal (MHashTable    *hash_table,
                              gconstpointer  key,
                              gboolean       notify)
{
  MHashNode *node;
  guint node_index;

  g_return_val_if_fail (hash_table != NULL, FALSE);

  node_index = m_hash_table_lookup_node (hash_table, key);
  node = &hash_table->nodes [node_index];

  /* m_hash_table_lookup_node() never returns a tombstone, so this is safe */
  if (!node->key_hash)
    return FALSE;

  m_hash_table_remove_node (hash_table, node, notify);
  m_hash_table_maybe_resize (hash_table);

  hash_table->version++;

  return TRUE;
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
 * Return value: %TRUE if the key was found and removed from the #MHashTable.
 **/
gboolean
m_hash_table_remove (MHashTable    *hash_table,
                     gconstpointer  key)
{
  return m_hash_table_remove_internal (hash_table, key, TRUE);
}

/**
 * m_hash_table_steal:
 * @hash_table: a #MHashTable.
 * @key: the key to remove.
 *
 * Removes a key and its associated value from a #MHashTable without
 * calling the key and value destroy functions.
 *
 * Return value: %TRUE if the key was found and removed from the #MHashTable.
 **/
gboolean
m_hash_table_steal (MHashTable    *hash_table,
                    gconstpointer  key)
{
  return m_hash_table_remove_internal (hash_table, key, FALSE);
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
void
m_hash_table_remove_all (MHashTable *hash_table)
{
  g_return_if_fail (hash_table != NULL);

  if (hash_table->nnodes != 0)
    hash_table->version++;

  m_hash_table_remove_all_nodes (hash_table, TRUE);
  m_hash_table_maybe_resize (hash_table);
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
void
m_hash_table_steal_all (MHashTable *hash_table)
{
  g_return_if_fail (hash_table != NULL);

  if (hash_table->nnodes != 0)
    hash_table->version++;

  m_hash_table_remove_all_nodes (hash_table, FALSE);
  m_hash_table_maybe_resize (hash_table);
}

/*
 * m_hash_table_foreach_remove_or_steal:
 * @hash_table: our #MHashTable
 * @func: the user's callback function
 * @user_data: data for @func
 * @notify: %TRUE if the destroy notify handlers are to be called
 *
 * Implements the common logic for m_hash_table_foreach_remove() and
 * m_hash_table_foreach_steal().
 *
 * Iterates over every node in the table, calling @func with the key
 * and value of the node (and @user_data).  If @func returns %TRUE the
 * node is removed from the table.
 *
 * If @notify is true then the destroy notify handlers will be called
 * for each removed node.
 */
static guint
m_hash_table_foreach_remove_or_steal (MHashTable *hash_table,
                                      GHRFunc	  func,
                                      gpointer    user_data,
                                      gboolean    notify)
{
  guint deleted = 0;
  gint i;

  for (i = 0; i < hash_table->size; i++)
    {
      MHashNode *node = &hash_table->nodes [i];

      if (node->key_hash > 1 && (* func) (node->key, node->value, user_data))
        {
          m_hash_table_remove_node (hash_table, node, notify);
          deleted++;
        }
    }

  m_hash_table_maybe_resize (hash_table);

  if (deleted > 0)
    hash_table->version++;

  return deleted;
}

/**
 * m_hash_table_foreach_remove:
 * @hash_table: a #MHashTable.
 * @func: the function to call for each key/value pair.
 * @user_data: user data to pass to the function.
 *
 * Calls the given function for each key/value pair in the #MHashTable.
 * If the function returns %TRUE, then the key/value pair is removed from the
 * #MHashTable. If you supplied key or value destroy functions when creating
 * the #MHashTable, they are used to free the memory allocated for the removed
 * keys and values.
 *
 * See #MHashTableIter for an alternative way to loop over the 
 * key/value pairs in the hash table.
 *
 * Return value: the number of key/value pairs removed.
 **/
guint
m_hash_table_foreach_remove (MHashTable *hash_table,
                             GHRFunc     func,
                             gpointer    user_data)
{
  g_return_val_if_fail (hash_table != NULL, 0);
  g_return_val_if_fail (func != NULL, 0);

  return m_hash_table_foreach_remove_or_steal (hash_table, func, user_data, TRUE);
}

/**
 * m_hash_table_foreach_steal:
 * @hash_table: a #MHashTable.
 * @func: the function to call for each key/value pair.
 * @user_data: user data to pass to the function.
 *
 * Calls the given function for each key/value pair in the #MHashTable.
 * If the function returns %TRUE, then the key/value pair is removed from the
 * #MHashTable, but no key or value destroy functions are called.
 *
 * See #MHashTableIter for an alternative way to loop over the 
 * key/value pairs in the hash table.
 *
 * Return value: the number of key/value pairs removed.
 **/
guint
m_hash_table_foreach_steal (MHashTable *hash_table,
                            GHRFunc     func,
                            gpointer    user_data)
{
  g_return_val_if_fail (hash_table != NULL, 0);
  g_return_val_if_fail (func != NULL, 0);

  return m_hash_table_foreach_remove_or_steal (hash_table, func, user_data, FALSE);
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
void
m_hash_table_foreach (MHashTable *hash_table,
                      GHFunc      func,
                      gpointer    user_data)
{
  gint i;

  g_return_if_fail (hash_table != NULL);
  g_return_if_fail (func != NULL);

  for (i = 0; i < hash_table->size; i++)
    {
      MHashNode *node = &hash_table->nodes [i];

      if (node->key_hash > 1)
        (* func) (node->key, node->value, user_data);
    }
}

/**
 * m_hash_table_find:
 * @hash_table: a #MHashTable.
 * @predicate:  function to test the key/value pairs for a certain property.
 * @user_data:  user data to pass to the function.
 *
 * Calls the given function for key/value pairs in the #MHashTable until
 * @predicate returns %TRUE.  The function is passed the key and value of
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
 * func evaluates to %TRUE. If no pair with the requested property is found,
 * %NULL is returned.
 *
 * Since: 2.4
 **/
gpointer
m_hash_table_find (MHashTable      *hash_table,
                   GHRFunc          predicate,
                   gpointer         user_data)
{
  gint i;

  g_return_val_if_fail (hash_table != NULL, NULL);
  g_return_val_if_fail (predicate != NULL, NULL);

  for (i = 0; i < hash_table->size; i++)
    {
      MHashNode *node = &hash_table->nodes [i];

      if (node->key_hash > 1 && predicate (node->key, node->value, user_data))
        return node->value;
    }

  return NULL;
}

/**
 * m_hash_table_size:
 * @hash_table: a #MHashTable.
 *
 * Returns the number of elements contained in the #MHashTable.
 *
 * Return value: the number of key/value pairs in the #MHashTable.
 **/
guint
m_hash_table_size (MHashTable *hash_table)
{
  g_return_val_if_fail (hash_table != NULL, 0);

  return hash_table->nnodes;
}

/**
 * m_hash_table_get_keys:
 * @hash_table: a #MHashTable
 *
 * Retrieves every key inside @hash_table. The returned data is valid
 * until @hash_table is modified.
 *
 * Return value: a #GList containing all the keys inside the hash
 *   table. The content of the list is owned by the hash table and
 *   should not be modified or freed. Use g_list_free() when done
 *   using the list.
 *
 * Since: 2.14
 */
GList *
m_hash_table_get_keys (MHashTable *hash_table)
{
  gint i;
  GList *retval;

  g_return_val_if_fail (hash_table != NULL, NULL);

  retval = NULL;
  for (i = 0; i < hash_table->size; i++)
    {
      MHashNode *node = &hash_table->nodes [i];

      if (node->key_hash > 1)
        retval = g_list_prepend (retval, node->key);
    }

  return retval;
}

/**
 * m_hash_table_get_values:
 * @hash_table: a #MHashTable
 *
 * Retrieves every value inside @hash_table. The returned data is
 * valid until @hash_table is modified.
 *
 * Return value: a #GList containing all the values inside the hash
 *   table. The content of the list is owned by the hash table and
 *   should not be modified or freed. Use g_list_free() when done
 *   using the list.
 *
 * Since: 2.14
 */
GList *
m_hash_table_get_values (MHashTable *hash_table)
{
  gint i;
  GList *retval;

  g_return_val_if_fail (hash_table != NULL, NULL);

  retval = NULL;
  for (i = 0; i < hash_table->size; i++)
    {
      MHashNode *node = &hash_table->nodes [i];

      if (node->key_hash > 1)
        retval = g_list_prepend (retval, node->value);
    }

  return retval;
}

//#define __M_HASH_C__
//#include "galiasdef.c"
