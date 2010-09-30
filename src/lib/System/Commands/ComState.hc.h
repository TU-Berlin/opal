/* hand-coded interface part of ComState */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

typedef struct sSTATEID {
    OBJ state;
} * STATEID;

#define pack_id(x) pack_pointer(x)
#define unpack_id(x) unpack_pointer(x)
    
