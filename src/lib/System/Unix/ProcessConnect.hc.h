/* hand-coded interface part of ProcessConnect */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/
typedef struct sCHANNEL {
  int swap;
  OBJ inName;
  int in;
  void * inFile;   /* actually FILE */
  OBJ outName;
  int out;
  void * outFile;  /* actually FILE */
} * CHANNEL;

#define pack_channel(chan) (pack_pointer(chan))
#define unpack_channel(chan) ((CHANNEL)unpack_pointer(chan))

extern int pconn_test_incoming(OBJ);
