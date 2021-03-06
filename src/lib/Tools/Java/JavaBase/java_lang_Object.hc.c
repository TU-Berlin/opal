/* generated by javabind version 1.1a */
#include "JavaVM.oc.h"

extern jclass javabind_class_java_lang_Object;
extern jclass javabind_class_java_lang_Class;
static jmethodID cId_AI1;
static jmethodID mId_AI2;
static jmethodID mId_AI3;
static jmethodID mId_AI4;
static jmethodID mId_AI5;
static jmethodID mId_AI6;
static jmethodID mId_AI7;
static jmethodID mId_AI8;
static jmethodID mId_AI9;
static jmethodID mId_AI10;

extern OBJ _Ajava_Alang_AObject_AI1(OBJ unit){
 OBJ y;
 { jvalue _argv[1];
  jobject _res;
  _res = javabind_NewObjectA(javabind_class_java_lang_Object,cId_AI1,_argv);
  javabind_catch_fail();
  javabind_fromObject(_res,y);}
 
 javabind_return(y);}

extern OBJ _Ajava_Alang_AObject_AI2(OBJ x1,OBJ unit){
 OBJ y;
 { jvalue _argv[2];
  jobject _res;
  javabind_asObject(x1,_argv[0].l);
  _res = javabind_CallObjectMethodA(_argv[0].l,mId_AI2,&_argv[1]);
  javabind_catch_fail();
  javabind_fromObject(_res,y);
  javabind_free_arg(x1);}
 
 javabind_return(y);}

extern OBJ _Ajava_Alang_AObject_AI3(OBJ x1,OBJ unit){
 OBJ y;
 { jvalue _argv[2];
  jint _res;
  javabind_asObject(x1,_argv[0].l);
  _res = javabind_CallIntMethodA(_argv[0].l,mId_AI3,&_argv[1]);
  javabind_catch_fail();
  javabind_fromInt(_res,y);
  javabind_free_arg(x1);}
 
 javabind_return(y);}

extern OBJ _Ajava_Alang_AObject_AI4(OBJ x1,OBJ x2,OBJ unit){
 OBJ y;
 { jvalue _argv[3];
  jboolean _res;
  javabind_asObject(x1,_argv[0].l);
  javabind_asObject(x2,_argv[1].l);
  _res = javabind_CallBooleanMethodA(_argv[0].l,mId_AI4,&_argv[1]);
  javabind_catch_fail();
  javabind_fromBoolean(_res,y);
  javabind_free_arg(x1);
  javabind_free_arg(x2);}
 
 javabind_return(y);}

extern OBJ _Ajava_Alang_AObject_AI5(OBJ x1,OBJ unit){
 OBJ y;
 { jvalue _argv[2];
  jobject _res;
  javabind_asObject(x1,_argv[0].l);
  _res = javabind_CallObjectMethodA(_argv[0].l,mId_AI5,&_argv[1]);
  javabind_catch_fail();
  javabind_fromString(_res,y);
  javabind_free_arg(x1);}
 
 javabind_return(y);}

extern OBJ _Ajava_Alang_AObject_AI6(OBJ x1,OBJ unit){
 OBJ y;
 { jvalue _argv[2];
  javabind_asObject(x1,_argv[0].l);
  javabind_CallVoidMethodA(_argv[0].l,mId_AI6,&_argv[1]);
  javabind_catch_fail();
  javabind_fromVoid(y);
  javabind_free_arg(x1);}
 
 javabind_return(y);}

extern OBJ _Ajava_Alang_AObject_AI7(OBJ x1,OBJ unit){
 OBJ y;
 { jvalue _argv[2];
  javabind_asObject(x1,_argv[0].l);
  javabind_CallVoidMethodA(_argv[0].l,mId_AI7,&_argv[1]);
  javabind_catch_fail();
  javabind_fromVoid(y);
  javabind_free_arg(x1);}
 
 javabind_return(y);}

extern OBJ _Ajava_Alang_AObject_AI8(OBJ x1,OBJ x2,OBJ unit){
 OBJ y;
 { jvalue _argv[3];
  javabind_asObject(x1,_argv[0].l);
  javabind_asLong(x2,_argv[1].j);
  javabind_CallVoidMethodA(_argv[0].l,mId_AI8,&_argv[1]);
  javabind_catch_fail();
  javabind_fromVoid(y);
  javabind_free_arg(x1);}
 
 javabind_return(y);}

extern OBJ _Ajava_Alang_AObject_AI9(OBJ x1,OBJ x2,OBJ x3,OBJ unit){
 OBJ y;
 { jvalue _argv[4];
  javabind_asObject(x1,_argv[0].l);
  javabind_asLong(x2,_argv[1].j);
  javabind_asInt(x3,_argv[2].i);
  javabind_CallVoidMethodA(_argv[0].l,mId_AI9,&_argv[1]);
  javabind_catch_fail();
  javabind_fromVoid(y);
  javabind_free_arg(x1);}
 
 javabind_return(y);}

extern OBJ _Ajava_Alang_AObject_AI10(OBJ x1,OBJ unit){
 OBJ y;
 { jvalue _argv[2];
  javabind_asObject(x1,_argv[0].l);
  javabind_CallVoidMethodA(_argv[0].l,mId_AI10,&_argv[1]);
  javabind_catch_fail();
  javabind_fromVoid(y);
  javabind_free_arg(x1);}
 
 javabind_return(y);}


static void init_const_Ajava_Alang_AObject(){
 cId_AI1 =
    javabind_GetMethodID(javabind_class_java_lang_Object,"<init>","()V");
 mId_AI2 =
    javabind_GetMethodID(javabind_class_java_lang_Object,"getClass","()Ljava/lang/Class;")
   ;
 mId_AI3 =
    javabind_GetMethodID(javabind_class_java_lang_Object,"hashCode","()I");
 mId_AI4 =
    javabind_GetMethodID(javabind_class_java_lang_Object,"equals","(Ljava/lang/Object;)Z")
   ;
 mId_AI5 =
    javabind_GetMethodID(javabind_class_java_lang_Object,"toString","()Ljava/lang/String;")
   ;
 mId_AI6 =
    javabind_GetMethodID(javabind_class_java_lang_Object,"notify","()V");
 mId_AI7 =
    javabind_GetMethodID(javabind_class_java_lang_Object,"notifyAll","()V");
 mId_AI8 = javabind_GetMethodID(javabind_class_java_lang_Object,"wait","(J)V")
   ;
 mId_AI9 =
    javabind_GetMethodID(javabind_class_java_lang_Object,"wait","(JI)V");
 mId_AI10 = javabind_GetMethodID(javabind_class_java_lang_Object,"wait","()V")
   ;}
