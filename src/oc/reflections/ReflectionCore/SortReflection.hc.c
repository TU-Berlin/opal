/*
  $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/oc/reflections/ReflectionCore/SortReflection.hc.c,v 1.1 1999-03-23 12:37:00 kd Exp $
  $Author: kd $
  $Date: 1999-03-23 12:37:00 $
  $State: Exp $

  $Locker:  $
  $Revision: 1.1 $
  $Name: not supported by cvs2svn $

  $Log: not supported by cvs2svn $
  Revision 1.1  1999/03/20 22:51:23  opaladm
  *** empty log message ***

  Revision 1.1  1998/12/06 20:20:46  silver
  Initial revision


*/

/* hand-coded implementation part of SortReflection */
/* coding scheme version acc-2.1 */

#define REFLECTION_HASH_TABLE_SIZE 997

OBJ hash_table [REFLECTION_HASH_TABLE_SIZE];

OBJ* magic_array = 0;
NAT  magic_array_size = 0;
NAT  next_position = 0;

extern OBJ _ASortReflection_AlookupPosition(OBJ x1) /* lookupPosition */
{
  /* Calculate the hash index: */
  NAT i;
  WORD hash_value = 0;
  {
    OBJ identifier = FLD(x1, 0);
    OBJ structure  = FLD(x1, 1);
    OBJ list       = FLD(x1, 2);
    
    WORD pos;
    WORD length;
    WORD hash_value_identifier = 0;
    WORD hash_value_structure  = 0;
    WORD hash_value_insts      = 0;
    
    /* Start with identifier */
    length = leng_denotation(identifier);
    for (pos=0; pos < length; pos ++)
      hash_value_identifier += data_denotation(identifier)[pos];
          
    /* Now structure */
    length = leng_denotation(structure);
    for (pos=0; pos < length; pos ++)
      hash_value_structure += data_denotation(structure)[pos];
      
    /* Now insts */
    while (!is_primitive(list))
      {
	hash_value_insts += unpack_nat (FLD(FLD(list, 0), 3)) + 13;
	hash_value_insts *= hash_value_insts;
	list = FLD(list, 1);
      }

    hash_value =
      (hash_value_identifier + (hash_value_structure << 16) + hash_value_insts)
      % REFLECTION_HASH_TABLE_SIZE;
  }

  /* Lookup */
  {
    OBJ lookup = hash_table [hash_value];

    while (!is_primitive(lookup))
      {
	NAT position = unpack_nat (FLD(lookup, 0));
	
	// Possible match!
	copy_structured (x1, 1);
	copy_structured (magic_array[position], 1);
	if (unpack_bool(_ASortReflection_AslowCompare (x1, magic_array[position])))
	  {
	    free_structured (x1, 1);
	    return FLD(lookup, 0);
	  }
	else
	  lookup = FLD(lookup, 1);
      }

    /* Ok, not found -> add! */
    alloc_small (2, lookup);
    FLD(lookup, 0) = pack_nat(next_position);
    FLD(lookup, 1) = hash_table [hash_value];
    hash_table [hash_value] = lookup;
  }
      
  /* Make room if necessary. */
  if (next_position >= magic_array_size)
    {
      magic_array_size += 256; 
      magic_array = (OBJ *) realloc (magic_array,
				     magic_array_size * sizeof(OBJ));

    }

  /* Remember */
  magic_array [next_position] = x1;
  return pack_nat (next_position++);
}

static init_const_ASortReflection()
{
  int i;
  
  /* On startup, we set the magic array to be empty: */
  magic_array = 0;
  magic_array_size = 0;

  /* The position in the array to use. */
  next_position = 0;

  /* Init the hash table */
  for (i=0; i<REFLECTION_HASH_TABLE_SIZE; i++)
    hash_table[i] = pack_word(0);
}


