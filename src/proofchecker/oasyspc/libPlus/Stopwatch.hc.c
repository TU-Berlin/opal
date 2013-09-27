/* hand-coded implementation part of Stopwatch */
/* coding scheme version acc-2.1 */

#include <sys/time.h>      
#include <unistd.h>
#include <unixconfig.h>
#include "Nat.h"
#include "Real.h"
#include "Com.h"
#include "Void.h"

static struct timeval start, iStart; 
static struct timezone tz;


extern OBJ _AStopwatch_Axstart(OBJ x1) /* xstart */
{OBJ r;

 gettimeofday(&start, &tz);

 FREE(x1,1);
 
 return_okay(__AVoid_Anil);}


extern OBJ _AStopwatch_Axelapsed(OBJ x1) /* xelapsed */
{OBJ r;
 struct timeval now;
 double v;

 gettimeofday(&now, &tz);
 v = ((double) (now.tv_sec - start.tv_sec)) + 
      ((double) (now.tv_usec - start.tv_usec) / 1000000);
 FREE(x1, 1);
 make_real(v, r);
 return_okay(r);}

extern OBJ _AStopwatch_Axnow(OBJ x1) /* xnow */
{OBJ r;
 struct timeval now;
 double v;

 gettimeofday(&now, &tz),
 v = (double) (now.tv_sec) + ((double) (now.tv_usec) / 1000000);
 FREE(x1, 1);
 make_real(v, r);
 return_okay(r);}

extern OBJ _AStopwatch_AiStart(OBJ x1) /* iStart */
{
 gettimeofday(&iStart, &tz);
 return x1;}

extern OBJ _AStopwatch_AiElapsed(OBJ x1, OBJ x2) /* iElapsed */
{OBJ r;
 struct timeval now;
 double v;

 gettimeofday(&now, &tz);
 v = ((double) 1000 * (now.tv_sec - iStart.tv_sec)) + 
      ((double) (now.tv_usec - iStart.tv_usec) / 1000);
 if (unpack_bool(x1)) {iStart = now;};
 FREE(x1, 1);
 FREE(x2, 1);
 make_real(v, r);
 
  
 return r;}


static void init_const_AStopwatch()
{
  gettimeofday(&start, &tz);
  gettimeofday(&iStart, &tz);
}
