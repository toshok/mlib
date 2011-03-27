#include <stdio.h>

#include "mhash.h"

MHashTable*
create_hashtable ()
{
  MHashTable *ht = m_hash_table_new (g_direct_hash, g_direct_equal);
  return ht;
}

void
test_hashtable (MHashTable *ht)
{
  printf ("yo!\n");

  GList *keys = m_hash_table_get_keys (ht);
  GList *l;

  for (l = keys; l; l = l->next) {
    printf ("key: %p\n", l->data);
  }
}
