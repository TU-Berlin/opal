/* hand-coded interface part of JavaVM */

#include "jni.oc.h"

#include "Int.oc.h"
#include "Char.oc.h"
#include "Real.oc.h"
#include "SmallReal.oc.h"
#include "Com.oc.h"


/* #undef JAVABIND_GLOBALREFS */

/* Representation of Java Objects */

typedef struct sJOBJECT {
    struct sFCELL header;
    jobject object;
} * JOBJECT;

#define jobject_size sizeof_foreign(struct sJOBJECT)

/* Global Variables */

extern JNIEnv *javabind_env;
extern OBJ javabind_freeafter;
extern jthrowable javabind_last_exception;
extern OBJ javabind_exception_ans;

/* Classes available by default */

extern jclass javabind_class_java_lang_String;

/* Cell flag assigned to null-arrays and null-denotations. */

#define null_sflag usr_sflag  /* FIXME: need to reserve sys_sflags in BUILTIN */


/* Allocation and free */

extern void _javabind_dispose(OBJ);

#define alloc_jobject(r)  alloc_foreign(jobject_size, _javabind_dispose,r)
#define copy_jobject(o,n) copy_structured(o,n)
#define free_jobject(o,n) free_structured(o,n)
#define decr_jobject(o,n) decr_structured(o,n)
#define excl_jobject(o,n) excl_structured(o,n)

#define get_jobject(o)   (((JOBJECT)(o))->object)
#define make_jobject(jo,r) {alloc_jobject(r); get_jobject(r)=jo;}

#define javabind_free_temp(x) {\
  if(x!=NULL)(*javabind_env)->DeleteLocalRef(javabind_env,x);\
}
	
#define javabind_free_arg(x) {\
  if (excl_jobject(x, 1)){\
    _javabind_dispose(x);\
  } else {\
    decr_jobject(x, 1);\
  }\
}


/* Type Conversions for JNI calls */

#define javabind_fromVoid(r) {r = NIL;}

extern jobject _javabind_asObject(OBJ);
extern OBJ _javabind_fromObject(jobject);
extern jobject _javabind_asObjectArray(int, jclass, jobject (*)(OBJ), OBJ);
extern OBJ _javabind_fromObjectArray(int, jclass, OBJ (*)(jobject), jobject);

#define javabind_asObject(x,j){j=get_jobject(x);}

#ifdef JAVABIND_GLOBALREFS
#define javabind_fromObject(j,x){\
  jobject __gj = (*javabind_env)->NewGlobalRef(javabind_env, j);\
  (*javabind_env)->DeleteLocalRef(javabind_env, j);\
  make_jobject(__gj,x);\
}
#else
#define javabind_fromObject(j,x){\
  make_jobject(j,x);\
}
#endif


#define javabind_asObjectArray(d,c,f,x,j) {j=_javabind_asObjectArray(d,c,f,x);}
#define javabind_fromObjectArray(d,c,f,j,x) {x=_javabind_fromObjectArray(d,c,f,j);}


extern jobject _javabind_asBooleanArray(OBJ);
extern OBJ _javabind_fromBooleanArray(jobject);
#define javabind_asBoolean(x,j)        {j = unpack_bool(x);}
#define javabind_fromBoolean(j,x)      {x = pack_clean_bool(j == JNI_TRUE);}
#define javabind_asBooleanArray(x,j)   {j=_javabind_asBooleanArray(x);}
#define javabind_fromBooleanArray(j,x) {x=_javabind_fromBooleanArray(j);}

extern jobject _javabind_asByteArray(OBJ);
extern OBJ _javabind_fromByteArray(jobject);
/* FIXME: real byte type */
#define javabind_asByte(x,j)        {j = unpack_int(x);} /* possible overflow! */
#define javabind_fromByte(j,x)      {x = pack_int(j);}
#define javabind_asByteArray(x,j)   {j=_javabind_asByteArray(x);}
#define javabind_fromByteArray(j,x) {x=_javabind_fromByteArray(j);}

extern jobject _javabind_asCharArray(OBJ);
extern OBJ _javabind_fromCharArray(jobject);
/* FIXME: widden char type to unsigned short */
#define javabind_asChar(x,j)        {j = unpack_char(x);}
#define javabind_fromChar(j,x)      {x = pack_char(j);} /* possible overflow! */
#define javabind_asCharArray(x,j)   {j=_javabind_asCharArray(x);}
#define javabind_fromCharArray(j,x) {x=_javabind_fromCharArray(j);}

extern jobject _javabind_asShortArray(OBJ);
extern OBJ _javabind_fromShortArray(jobject);
/* FIXME: real short type */
#define javabind_asShort(x,j)        {j = unpack_int(x);} /* possible overflow! */
#define javabind_fromShort(j,x)      {x = pack_int(j);}
#define javabind_asShortArray(x,j)   {j=_javabind_asShortArray(x);}
#define javabind_fromShortArray(j,x) {x=_javabind_fromShortArray(j);}

extern jobject _javabind_asIntArray(OBJ);
extern OBJ _javabind_fromIntArray(jobject);
/* FIXME: real 32-bit integer type */
#define javabind_asInt(x,j)        {j = unpack_int(x);} 
#define javabind_fromInt(j,x)      {x = pack_int(j);} /* possible overflow ! */
#define javabind_asIntArray(x,j)   {j=_javabind_asIntArray(x);}
#define javabind_fromIntArray(j,x) {x=_javabind_fromIntArray(j);}

extern jobject _javabind_asLongArray(OBJ);
extern OBJ _javabind_fromLongArray(jobject);
/* FIXME: real 64-bit integer type */
#define javabind_asLong(x,j)        {j = atol(data_denotation(x));\
                                     free_denotation(x,1);}
#define javabind_fromLong(j,x)      {char __buf[15];\
                                     sprintf(__buf,"%ld",j);\
				     x = make_denotation(__buf);}
#define javabind_asLongArray(x,j)   {j=_javabind_asLongArray(x);}
#define javabind_fromLongArray(j,x) {x=_javabind_fromLongArray(j);}

extern jobject _javabind_asFloatArray(OBJ);
extern OBJ _javabind_fromFloatArray(jobject);
/* FIXME: real 32-bit float type */
#define javabind_asFloat(x,j)        {get_sreal(x,j);}
                                                     /* possible overflow ! */
#define javabind_fromFloat(j,x)      {make_sreal((double)j,x);} 
#define javabind_asFloatArray(x,j)   {j=_javabind_asFloatArray(x);}
#define javabind_fromFloatArray(j,x) {x=_javabind_fromFloatArray(j);}

    
extern jobject _javabind_asDoubleArray(OBJ);
extern OBJ _javabind_fromDoubleArray(jobject);
#define javabind_asDouble(x,j)        {j = ((REAL)(x))->value; free_real(x,1);}
#define javabind_fromDouble(j,x)      {make_real(j,x);} 
#define javabind_asDoubleArray(x,j)   {j=_javabind_asDoubleArray(x);}
#define javabind_fromDoubleArray(j,x) {x=_javabind_fromDoubleArray(j);}


extern jobject _javabind_asString(OBJ);
extern OBJ _javabind_fromString(jobject);
#define javabind_asString(x,j)        {j=_javabind_asString(x);}
#define javabind_fromString(j,x)      {x=_javabind_fromString(j);}


/* Abstractions of JNI Functions. */

extern jclass javabind_FindClass(const char *name);
extern jmethodID javabind_GetMethodID(jclass clazz, const char *name, 
				     const char *sign);
extern jmethodID javabind_GetStaticMethodID(jclass clazz, const char *name, 
					   const char *sign);
extern jfieldID javabind_GetFieldID(jclass clazz, const char *name, 
				     const char *sign);
extern jfieldID javabind_GetStaticFieldID(jclass clazz, const char *name, 
					 const char *sign);


#define JNIPRIM(TYPE,QUAL)\
  extern TYPE javabind_Get##QUAL##Field(jobject ob, jfieldID id);\
  extern void javabind_Set##QUAL##Field(jobject ob, jfieldID id, TYPE x);\
  extern TYPE javabind_GetStatic##QUAL##Field(jclass clazz, jfieldID id);\
  extern void javabind_SetStatic##QUAL##Field(jclass clazz, jfieldID id, TYPE x);\
  extern TYPE javabind_Call##QUAL##MethodA(jobject ob, jmethodID id, jvalue argv[]);\
  extern TYPE javabind_CallStatic##QUAL##MethodA(jclass clazz, \
					jmethodID id, jvalue argv[]);

JNIPRIM(jboolean,Boolean)
JNIPRIM(jchar,Char)
JNIPRIM(jbyte,Byte)
JNIPRIM(jshort,Short)
JNIPRIM(jint,Int)
JNIPRIM(jlong,Long)
JNIPRIM(jfloat,Float)
JNIPRIM(jdouble,Double)
JNIPRIM(jobject,Object)

extern jobject javabind_NewObjectA(jclass clazz, jmethodID id, jvalue argv[]);
extern void javabind_CallVoidMethodA(jobject ob, jmethodID id, jvalue argv[]);
extern void javabind_CallStaticVoidMethodA(jclass clazz, jmethodID id, jvalue argv[]);

  
/* Exceptions and Returning */

extern OBJ _javabind_fail();
extern void _javabind_abort();

#define javabind_return(x){\
    return_okay(x);\
}

#define javabind_immutable_return(x){\
    return x;\
}

#define javabind_catch_abort(){\
    if ((*javabind_env)->ExceptionOccurred(javabind_env) != NULL){\
	_javabind_abort();\
    }\
}

#define javabind_catch_fail(){\
    if ((*javabind_env)->ExceptionOccurred(javabind_env) != NULL){\
	return _javabind_fail();\
    }\
}



/* Casting */

#define javabind_upcast(x,y) {(y) = (x);}

#define javabind_downcast(x,clazz,y){\
    if (!(*javabind_env)->IsInstanceOf(javabind_env, get_jobject(x), clazz)){\
	HLT("narrowing of java object failed");\
    }\
    (y) = (x);\
}
    
#define javabind_downtest(x,clazz,y){\
    jboolean _b = \
	(*javabind_env)->IsInstanceOf(javabind_env, get_jobject(x), clazz);\
    (y) = pack_bool(_b);\
}
    

