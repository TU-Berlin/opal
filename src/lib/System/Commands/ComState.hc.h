/* hand-coded interface part of ComState */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 16:00:17 $ ($Revision: 1.1.1.1 $)
*/

typedef struct sSTATEID {
    OBJ state;
} * STATEID;

#define pack_id(x) pack_pointer(x)
#define unpack_id(x) unpack_pointer(x)
    
