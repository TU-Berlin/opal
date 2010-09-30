/* hand-coded implementation part of ComStateWith */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

#include "Com.h"
#include "ComState.h"

extern OBJ _AComStateWith_AWith(OBJ Id,OBJ Trans,OBJ Dummy) /* With */ {
    STATEID id = unpack_id(Id);
    OBJ Tmp, Res;

    Tmp = EVAL1(Trans, id->state);
    id->state = FLD1(Tmp, 1); Res = FLD1(Tmp, 2);
    dispose_structured_flat(Tmp);

    return_okay(Res);
}


extern OBJ _AComStateWith_AGet(OBJ Id,OBJ Fun,OBJ Dummy) /* Get */ {
    STATEID id = unpack_id(Id);
    OBJ Res;

    copy_some(id->state, 1);
    Res = EVAL1(Fun, id->state);

    return_okay(Res);
}

static init_const_AComStateWith()
{}
