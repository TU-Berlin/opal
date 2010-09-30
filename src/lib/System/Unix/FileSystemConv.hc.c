/* hand-coded implementation part of FileSystemConv */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

#include <unixconfig.h>
#include "FileSystem.h"


extern OBJ _AFileSystemConv_Ahc_Aconvinode(OBJ x1) /* hc_convinode */
{OBJ r;
  (void)sprintf(charbuf,"%lu",(unsigned long)(((INODE)(x1))->value));
  free_inode(x1,1);
  r=make_denotation(charbuf);
 return r;}

extern OBJ _AFileSystemConv_Ahc_Aconvdevice(OBJ x1) /* hc_convdevice */
{OBJ r;
  (void)sprintf(charbuf,"%lu",(unsigned long)(((DEVICE)(x1))->value));
  free_device(x1,1);
  r=make_denotation(charbuf);
 return r;}

static init_const_AFileSystemConv()
{
 init_AFileSystem();
}
