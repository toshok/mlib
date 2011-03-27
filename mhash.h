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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
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

#include <glib.h>

#ifndef __M_HASH_H__
#define __M_HASH_H__

G_BEGIN_DECLS

typedef struct _MHashTable  MHashTable;

typedef gboolean  (*MHRFunc)  (gpointer  key,
                               gpointer  value,
                               gpointer  user_data);

typedef gboolean        (*MEqualFunc)           (gconstpointer  a,
                                                 gconstpointer  b);
typedef void            (*MDestroyNotify)       (gpointer       data);
typedef guint           (*MHashFunc)            (gconstpointer  key);
typedef void            (*MHFunc)               (gpointer       key,
                                                 gpointer       value,
                                                 gpointer       user_data);

typedef struct _MHashTableIter MHashTableIter;

struct _MHashTableIter
{
  /*< private >*/
  gpointer	dummy1;
  gpointer	dummy2;
  gpointer	dummy3;
  int		dummy4;
  gboolean	dummy5;
  gpointer	dummy6;
};

/* Hash tables
 */
MHashTable* m_hash_table_new		   (MHashFunc	    hash_func,
					    GEqualFunc	    key_equal_func);
MHashTable* m_hash_table_new_full      	   (MHashFunc	    hash_func,
					    GEqualFunc	    key_equal_func,
					    GDestroyNotify  key_destroy_func,
					    GDestroyNotify  value_destroy_func);
void	    m_hash_table_destroy	   (MHashTable	   *hash_table);
void	    m_hash_table_insert		   (MHashTable	   *hash_table,
					    gpointer	    key,
					    gpointer	    value);
void        m_hash_table_replace           (MHashTable     *hash_table,
					    gpointer	    key,
					    gpointer	    value);
gboolean    m_hash_table_remove		   (MHashTable	   *hash_table,
					    gconstpointer   key);
void        m_hash_table_remove_all        (MHashTable     *hash_table);
gboolean    m_hash_table_steal             (MHashTable     *hash_table,
					    gconstpointer   key);
void        m_hash_table_steal_all         (MHashTable     *hash_table);
gpointer    m_hash_table_lookup		   (MHashTable	   *hash_table,
					    gconstpointer   key);
gboolean    m_hash_table_lookup_extended   (MHashTable	   *hash_table,
					    gconstpointer   lookup_key,
					    gpointer	   *orig_key,
					    gpointer	   *value);
void	    m_hash_table_foreach	   (MHashTable	   *hash_table,
					    MHFunc	    func,
					    gpointer	    user_data);
gpointer    m_hash_table_find	           (MHashTable	   *hash_table,
					    MHRFunc	    predicate,
					    gpointer	    user_data);
guint	    m_hash_table_foreach_remove	   (MHashTable	   *hash_table,
					    MHRFunc	    func,
					    gpointer	    user_data);
guint	    m_hash_table_foreach_steal	   (MHashTable	   *hash_table,
					    MHRFunc	    func,
					    gpointer	    user_data);
guint	    m_hash_table_size		   (MHashTable	   *hash_table);
GList *     m_hash_table_get_keys          (MHashTable     *hash_table);
GList *     m_hash_table_get_values        (MHashTable     *hash_table);

void        m_hash_table_iter_init         (MHashTableIter *iter,
					    MHashTable     *hash_table);
gboolean    m_hash_table_iter_next         (MHashTableIter *iter,
					    gpointer       *key,
					    gpointer       *value);
MHashTable* m_hash_table_iter_get_hash_table (MHashTableIter *iter);
void        m_hash_table_iter_remove       (MHashTableIter *iter);
void        m_hash_table_iter_steal        (MHashTableIter *iter);

/* keeping hash tables alive */
MHashTable* m_hash_table_ref   		   (MHashTable 	   *hash_table);
void        m_hash_table_unref             (MHashTable     *hash_table);

#ifndef G_DISABLE_DEPRECATED

/* The following two functions are deprecated and will be removed in
 * the next major release. They do no good. */
#define m_hash_table_freeze(hash_table) ((void)0)
#define m_hash_table_thaw(hash_table) ((void)0)

#endif /* G_DISABLE_DEPRECATED */

/* Hash Functions
 */
gboolean g_str_equal (gconstpointer  v1,
                      gconstpointer  v2);
guint    g_str_hash  (gconstpointer  v);

gboolean g_int_equal (gconstpointer  v1,
                      gconstpointer  v2);
guint    g_int_hash  (gconstpointer  v);

gboolean g_int64_equal (gconstpointer  v1,
                        gconstpointer  v2);
guint    g_int64_hash  (gconstpointer  v);

gboolean g_double_equal (gconstpointer  v1,
                         gconstpointer  v2);
guint    g_double_hash  (gconstpointer  v);

/* This "hash" function will just return the key's address as an
 * unsigned integer. Useful for hashing on plain addresses or
 * simple integer values.
 * Passing NULL into m_hash_table_new() as MHashFunc has the
 * same effect as passing g_direct_hash().
 */
guint    g_direct_hash  (gconstpointer  v) G_GNUC_CONST;
gboolean g_direct_equal (gconstpointer  v1,
                         gconstpointer  v2) G_GNUC_CONST;

G_END_DECLS

#endif /* __M_HASH_H__ */
