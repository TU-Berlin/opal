/*
  $Header: /home/florenz/opal/home_uebb_CVS/CVS/ocs/src/oc/reflections/ReflectionCore/REFLECTION.hc.c,v 1.1 1999-03-23 12:36:50 kd Exp $
  $Author: kd $
  $Date: 1999-03-23 12:36:50 $
  $State: Exp $

  $Locker:  $
  $Revision: 1.1 $
  $Name: not supported by cvs2svn $

  $Log: not supported by cvs2svn $
  Revision 1.1  1999/03/20 22:51:23  opaladm
  *** empty log message ***

  Revision 1.5  1998/12/19 17:41:13  silver
  Bugfix for init call.

  Revision 1.4  1998/12/06 20:30:04  silver
  First version with compiler extentions.

  Revision 1.3  1998/11/12 13:20:10  silver
  Implementation of hashed sortReflections.

  Revision 1.2  1998/11/03 16:56:53  silver
  Wrote fast compare mechanism.

  Revision 1.1  1998/10/09 15:56:43  silver
  Initial revision

*/

/* hand-coded implementation part of REFLECTION */
/* coding scheme version acc-2.1 */

#include "SortReflection.h"

extern OBJ _AREFLECTION_Asort(OBJ x1,OBJ x2,OBJ x3) /* sort */
{
  /* Remap! */
  return _ASortReflection_Asort (x1, x2, x3);
}

static init_const_AREFLECTION()
{
  init_ASortReflection();
}


