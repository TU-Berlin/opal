/* hand-coded implementation part of Cache */
/* coding scheme version acc-2.1 */


#include "Com.h"
#include "ComState.h"
#include "Map.h"
#include "Option.h"


extern OBJ _ACache_Acache(OBJ x1,OBJ x2) /* cache */
{OBJ r;
 CPCLS(__AMap_5,1);
 CPCLS(x1,1);
 {OBJ x3;
  x3=(*(OBJ(*)(OBJ,OBJ))METHOD(__AMap_5,1))(__AMap_5,x1);
  ACache_Acache_O1(x1,x2,x3,r);}
 return r;}

extern OBJ _ACache_Acache_O1(OBJ x1,OBJ x2,OBJ x3) /* cache,1 */
{OBJ r;
 CPCLS(__ACache_Acache_O2,1);
 CPCLS(__ACom_25,1);
 CPCLS(__AComState_3,1);
 {OBJ x4;OBJ x5;
  x4=(*(OBJ(*)(OBJ,OBJ))METHOD(__AComState_3,1))(__AComState_3,x3);
  x5=(*(OBJ(*)(OBJ,OBJ))METHOD(__ACom_25,1))(__ACom_25,x4);
  r=(*(OBJ(*)(OBJ,OBJ,OBJ,OBJ))METHOD(__ACache_Acache_O2,3))(__ACache_Acache_O2,x1,x5,x2);}
 return r;}

extern OBJ _ACache_S1(OBJ x1,OBJ x2,OBJ x3) /* ! */
{OBJ r;
 CPCLS(x1,1);
 CPPRD(x2,1);
 {OBJ x4;
  ACache_Acache_(x1,x2,x4);
  if(ISTGPRM(x4,1)){
   CPCLS(x1,3);
   CPPRD(x2,1);
   COPY(x3,1);
   CPCLS(__ACom_25,1);
   CPCLS(__AMap_18,1);
   CPCLS(__AOption_3,1);
   CPCLS(__AComState_6,1);
   {OBJ x5;OBJ x6;OBJ x7;OBJ x8;OBJ x9;OBJ x10;
    ACache_Amap(x1,x2,x5);
    COPY(x5,1);
    ACache_Anew(x1,x2,x6);
    x7=(*(OBJ(*)(OBJ,OBJ))METHOD(__AComState_6,1))(__AComState_6,x5);
    x8=(*(OBJ(*)(OBJ,OBJ))METHOD(__ACom_25,1))(__ACom_25,x7);
    x9=(*(OBJ(*)(OBJ,OBJ,OBJ,OBJ))METHOD(__AMap_18,3))(__AMap_18,x1,x8,x3);
    COPY(x9,1);
    x10=(*(OBJ(*)(OBJ,OBJ))METHOD(__AOption_3,1))(__AOption_3,x9);
    if(ISTGPRM(x10,1)){
     FRCLS(x1,1);
     FREE(x3,1);
     FREE(x5,1);
     FRCLS(x6,1);
     CPCLS(__AOption_4,1);
     r=(*(OBJ(*)(OBJ,OBJ))METHOD(__AOption_4,1))(__AOption_4,x9);
    }else{
     FREE(x9,1);
     COPY(x3,1);
     CPCLS(__ACom_25,1);
     {OBJ x11;OBJ x12;
      x11=(*(OBJ(*)(OBJ,OBJ))METHOD(x6,1))(x6,x3);
      ACache_Aadd(x1,x5,x3,x11,x12);
      r=(*(OBJ(*)(OBJ,OBJ))METHOD(__ACom_25,1))(__ACom_25,x12);}}}
  }else{
   HLT("Cache at <unknown> : missing else in !\'Cache:cache**dom->codom");}}
 return r;}

static init_const_ACache()
{}
