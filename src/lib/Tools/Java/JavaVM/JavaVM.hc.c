/* hand-coded implementation part of JavaVM */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date: 1998-06-16 16:00:28 $ ($Revision: 1.1.1.1 $)
*/
#include "unixconfig.h"
#include "Array.h"
#include "Denotation.h"

/* ---------------------------------------------------------------------- */
/* Variables */

JNIEnv *javabind_env;
OBJ javabind_freeafter;
jthrowable javabind_last_exception;
OBJ javabind_exception_ans;

static JavaVM *jvm;

jclass javabind_class_java_lang_String;
jclass javabind_class_java_lang_Object;
jclass javabind_class_java_lang_Class;
jmethodID javabind_equals_method;
static jmethodID javabind_getName_method;


/* ---------------------------------------------------------------------- */
/* Forward declarations */

static jclass array_type(int dim, jclass);


/* ---------------------------------------------------------------------- */
/* Initialization */

static init_const_AJavaVM(){
    JDK1_1InitArgs vm_args;
    jint res;
    JNI_GetDefaultJavaVMInitArgs(&vm_args);
    vm_args.classpath = getenv("CLASSPATH");
    res = JNI_CreateJavaVM(&jvm,&javabind_env,&vm_args);
    if (res < 0) {
        HLT("Can't create Java VM");
    }
    javabind_freeafter = NULL;
    javabind_last_exception = NULL;
    javabind_exception_ans = declare_failure_answer("JavaVM exception");

    javabind_class_java_lang_String = javabind_FindClass("java/lang/String");
    javabind_class_java_lang_Object = javabind_FindClass("java/lang/Object");
    javabind_class_java_lang_Class = javabind_FindClass("java/lang/Class");
    javabind_equals_method = 
      javabind_GetMethodID(javabind_class_java_lang_Object,
			   "equals",
			   "(Ljava/lang/Object;)Z");
    if (javabind_equals_method == NULL){
      HLT("cannot bind Object.equals");
    }

    javabind_getName_method = 
      javabind_GetMethodID(javabind_class_java_lang_Class,
			   "getName",
			   "()Ljava/lang/String;");
    if (javabind_getName_method == NULL){
      HLT("cannot bind Class.getNames");
    }

}

/* ---------------------------------------------------------------------- */
/* Garbage Collection */

extern void _javabind_dispose(OBJ o){
    jobject jo = get_jobject(o);
    if (jo != NULL){
      (*javabind_env)->DeleteGlobalRef(javabind_env, jo);
    }
    dispose_foreign_mem(o,jobject_size);
}

#define freeafter(){\
    while (javabind_freeafter != NULL){\
	OBJ o = javabind_freeafter;\
        jobject jo = get_jobject(o);\
	javabind_freeafter = _link(_header(javabind_freeafter));\
        if (jo != NULL){\
	  (*javabind_env)->DeleteGlobalRef(javabind_env, jo);\
	}\
	dispose_foreign_mem(o,jobject_size);\
    }\
}
	

/* ---------------------------------------------------------------------- */
/* JNI Call Abstractions */

extern jclass javabind_FindClass(const char *name){
    jclass c;
    c = (*javabind_env)->FindClass(javabind_env, name);
    javabind_catch_abort();
    if (c == NULL){
	HLT("JavaVM: class not found but no exception??");
    }
    return c;
}

extern jmethodID javabind_GetMethodID(jclass clazz, const char *name, 
				     const char *sign){
    jmethodID id;
    id = (*javabind_env)->GetMethodID(javabind_env, clazz, name, sign);
    javabind_catch_abort();
    if (id == NULL){
	HLT("JavaVM: method not found but no exception??");
    }
    return id;
}

extern jmethodID javabind_GetStaticMethodID(jclass clazz, const char *name, 
					   const char *sign){
    jmethodID id;
    id = (*javabind_env)->GetStaticMethodID(javabind_env, clazz, name, sign);
    javabind_catch_abort();
    if (id == NULL){
	HLT("JavaVM: static method not found but no exception??");
    }
    return id;
}

extern jfieldID javabind_GetFieldID(jclass clazz, const char *name, 
				   const char *sign){
    jfieldID id;
    id = (*javabind_env)->GetFieldID(javabind_env, clazz, name, sign);
    javabind_catch_abort();
    if (id == NULL){
	HLT("JavaVM: field not found but no exception??");
    }
    return id;
}

extern jfieldID javabind_GetStaticFieldID(jclass clazz, const char *name, 
					 const char *sign){
    jfieldID id;
    id = (*javabind_env)->GetStaticFieldID(javabind_env, clazz, name, sign);
    javabind_catch_abort();
    if (id == NULL){
	HLT("JavaVM: field not found but no exception??");
    }
    return id;
}


#define JNIPRIMDEF(TYPE,QUAL)\
  extern TYPE javabind_Get##QUAL##Field(jobject ob, jfieldID id){\
    TYPE res = (*javabind_env)->Get##QUAL##Field(javabind_env, ob, id);\
    freeafter();\
    return res;\
  }\
  extern void javabind_Set##QUAL##Field(jobject ob, jfieldID id, TYPE x){\
    (*javabind_env)->Set##QUAL##Field(javabind_env, ob, id, x);\
    freeafter();\
  }\
  extern TYPE javabind_GetStatic##QUAL##Field(jclass clazz, jfieldID id){\
    TYPE res = (*javabind_env)->GetStatic##QUAL##Field(javabind_env,\
						      clazz, id);\
    freeafter();\
    return res;\
  }\
  extern void javabind_SetStatic##QUAL##Field(jclass clazz, jfieldID id, \
					     TYPE x){\
    (*javabind_env)->SetStatic##QUAL##Field(javabind_env, clazz, id, x);\
    freeafter();\
  }\
  extern TYPE javabind_Call##QUAL##MethodA(jobject ob, jmethodID id, \
					  jvalue argv[]){\
    TYPE res = (*javabind_env)->Call##QUAL##MethodA(javabind_env, ob, \
						   id, argv);\
    freeafter();\
    return res;\
  }\
  extern TYPE javabind_CallStatic##QUAL##MethodA(jclass clazz, jmethodID id, \
						jvalue argv[]){\
    TYPE res = (*javabind_env)->CallStatic##QUAL##MethodA(javabind_env, \
							 clazz, id, argv);\
    freeafter();\
    return res;\
  }

JNIPRIMDEF(jboolean,Boolean)
JNIPRIMDEF(jchar,Char)
JNIPRIMDEF(jbyte,Byte)
JNIPRIMDEF(jshort,Short)
JNIPRIMDEF(jint,Int)
JNIPRIMDEF(jlong,Long)
JNIPRIMDEF(jfloat,Float)
JNIPRIMDEF(jdouble,Double)
JNIPRIMDEF(jobject,Object)


extern jobject javabind_NewObjectA(jclass clazz, jmethodID id, jvalue argv[]){
    jobject res = (*javabind_env)->NewObjectA(javabind_env, clazz, id, argv);
    freeafter();
    return res;
}

extern void javabind_CallVoidMethodA(jobject ob, jmethodID id, jvalue argv[]){
    (*javabind_env)->CallVoidMethodA(javabind_env, ob, id, argv);
    freeafter();
}

extern void javabind_CallStaticVoidMethodA(jclass clazz, jmethodID id, 
					  jvalue argv[]){
    (*javabind_env)->CallStaticVoidMethodA(javabind_env, clazz, id, argv);
    freeafter();
}

		     

/* ---------------------------------------------------------------------- */
/* Exception Handling */

extern OBJ _javabind_fail(){
    /* DEBUG */
    /* (*javabind_env)->ExceptionDescribe(javabind_env); */
    if (javabind_last_exception != NULL){
	(*javabind_env)->DeleteGlobalRef(javabind_env,javabind_last_exception);
    }
    javabind_last_exception = (*javabind_env)->ExceptionOccurred(javabind_env);
    /*
    jthrowable t;
    javabind_last_exception = (*javabind_env)->NewGlobalRef(javabind_env,t);
    (*javabind_env)->DeleteLocalRef(javabind_env, t);
    */
    (*javabind_env)->ExceptionClear(javabind_env);
    return_fail(javabind_exception_ans);
}
    
extern void _javabind_abort(){
    (*javabind_env)->ExceptionDescribe(javabind_env);
    HLT("JavaVM: exception in immutable context, aborting.");
}



/* ---------------------------------------------------------------------- */
/* String Conversions */


extern jobject _javabind_asString(OBJ str){
    jstring jstr;
    jstr = (*javabind_env)->NewStringUTF(javabind_env, data_denotation(str));
    free_denotation(str,1);
    return jstr;
}

extern OBJ _javabind_fromString(jobject jstr){
    OBJ str;
    if (jstr != NULL){
      jsize l,i;
      jboolean isCopy;
      const jbyte *bytes;
      l = (*javabind_env)->GetStringUTFLength(javabind_env, jstr);
      bytes = (*javabind_env)->GetStringUTFChars(javabind_env, jstr, &isCopy);
      str = alloc_denotation(l);
      for (i = 0; i < l; i++){
  	data_denotation(str)[i] = bytes[i];
      }
      (*javabind_env)->ReleaseStringUTFChars(javabind_env, jstr, bytes);
      return str;
    } else {
      str = make_denotation("");
      set_sflag(str, null_sflag);
      return str;
    }
}

/* ---------------------------------------------------------------------- */
/* Code entries for Object Conversions (used for object arrays) */

extern jobject _javabind_asObject(OBJ x){
    jobject r;
    javabind_asObject(x,r);
    return r;
}

extern OBJ _javabind_fromObject(jobject x){
    OBJ r;
    javabind_fromObject(x,r);
    return r;
}


/* ---------------------------------------------------------------------- */
/* Array Conversions */

extern jobject _javabind_asObjectArray(int dim, jclass clazz, 
				      jobject (*conv)(OBJ),
				      OBJ arr){
   jsize leng,i; jarray jarr;
   leng = leng_array(arr);
   jarr = (*javabind_env)->NewObjectArray(javabind_env, leng,
					 array_type(dim-1, clazz),
					 NULL);
   javabind_catch_abort();
   for (i = 0; i < leng; i++){
       OBJ elem = data_array(arr)[i];
       jobject jo;
       copy_some(elem,1);
       if (dim > 1){
         jo = _javabind_asObjectArray(dim-1, clazz,
	 			      conv,
				      elem);
       } else {
         jo = (*conv)(elem);
       }
       (*javabind_env)->SetObjectArrayElement(javabind_env,
					     jarr,
					     i,
					     jo);
       javabind_catch_abort();
   }
   free_array(arr,1);
   return jarr;
}       
       
extern OBJ _javabind_fromObjectArray(int dim, jclass clazz,
				    OBJ (*conv)(jobject),
				    jarray jarr){
 OBJ arr;
 if (jarr != NULL){
   jsize leng,i; 
   leng = (*javabind_env)->GetArrayLength(javabind_env, jarr);
   arr = alloc_array(leng);
   for (i = 0; i < leng; i++){
       jobject jo = 
	   (*javabind_env)->GetObjectArrayElement(javabind_env,
						 jarr,
						 i);
       javabind_catch_abort();
       if (dim > 1){
         data_array(arr)[i] = 
	   _javabind_fromObjectArray(dim-1,
	 			     clazz,
				     conv, jo);
       } else {
         data_array(arr)[i] = (*conv)(jo);
       }
   }
   (*javabind_env)->DeleteLocalRef(javabind_env, jarr);
   return arr;
 } else {
   arr = alloc_array(0);
   set_sflag(arr, null_sflag);
   return arr;
 }
} 

#define PRIMARRAYCONV(TYPE,QUAL,ASCONV,FROMCONV) \
extern jobject _javabind_as##QUAL##Array(OBJ arr){\
   jsize leng,i; jarray jarr;\
   TYPE *buf;\
   leng = leng_array(arr);\
   jarr = (*javabind_env)->New##QUAL##Array(javabind_env, leng);\
   buf = (*javabind_env)->Get##QUAL##ArrayElements(javabind_env, jarr, NULL);\
   javabind_catch_abort();\
   for (i = 0; i < leng; i++){\
       OBJ elem = data_array(arr)[i];\
       copy_some(elem, 1);\
       ##ASCONV##(elem, buf[i]);\
   }\
   free_array(arr,1);\
   (*javabind_env)->Release##QUAL##ArrayElements(javabind_env, jarr, buf,\
						 0);\
   return jarr;\
}\
extern OBJ _javabind_from##QUAL##Array(jarray jarr){\
 OBJ arr;\
 if (jarr != NULL){\
   jsize leng,i; \
   TYPE *buf;\
   leng = (*javabind_env)->GetArrayLength(javabind_env, jarr);\
   arr = alloc_array(leng);\
   buf = (*javabind_env)->Get##QUAL##ArrayElements(javabind_env, jarr, NULL);\
   for (i = 0; i < leng; i++){\
     ##FROMCONV##(buf[i], data_array(arr)[i]);\
   }\
   (*javabind_env)->Release##QUAL##ArrayElements(javabind_env, jarr, buf,\
						 0);\
   return arr;\
 } else {\
   arr = alloc_array(0);\
   set_sflag(arr, null_sflag);\
   return arr;\
 }\
}

PRIMARRAYCONV(jboolean,Boolean,javabind_asBoolean,javabind_fromBoolean)
PRIMARRAYCONV(jchar,Char,javabind_asChar,javabind_fromChar)
PRIMARRAYCONV(jbyte,Byte,javabind_asByte,javabind_fromByte)
PRIMARRAYCONV(jshort,Short,javabind_asShort,javabind_fromShort)
PRIMARRAYCONV(jint,Int,javabind_asInt,javabind_fromInt)
PRIMARRAYCONV(jlong,Long,javabind_asLong,javabind_fromLong)
PRIMARRAYCONV(jfloat,Float,javabind_asFloat,javabind_fromFloat)
PRIMARRAYCONV(jdouble,Double,javabind_asDouble,javabind_fromDouble)



/* ---------------------------------------------------------------------- */
/* Object equality */


extern OBJ _AJavaVM_AEQUALS(OBJ x1,OBJ x2) /* EQUALS */
{
  OBJ y;
  jboolean res;
  jvalue argv[2];
  javabind_asObject(x1,argv[0].l);
  javabind_asObject(x2,argv[1].l);
  res = javabind_CallBooleanMethodA(argv[0].l, javabind_equals_method,
				    &argv[1]);
  javabind_catch_abort();
  javabind_fromBoolean(res, y);
  javabind_immutable_return(y);
}

/* ---------------------------------------------------------------------- */
/* Retrieving array element type */


static jclass array_type(int dim, jclass clazz){
    if (dim == 0){
	return clazz;
    } else {
        /* FIXME: this is a bit inefficient */
	OBJ y;
	OBJ den;
	jobject res;
	jvalue argv[1];
	char * c;
	res = javabind_CallObjectMethodA(clazz, javabind_getName_method,
					 &argv[0]);
	javabind_catch_abort();
	javabind_fromString(res, den);
	c = data_denotation(den);
	while (*c){
	    if (*c == '.') *c = '/';
	    c++;
	}
	den = _ADenotation_Spp(make_denotation("L"), den);
	den = _ADenotation_Spp(den, make_denotation(";"));
	while (dim > 0){
	    den = _ADenotation_Spp(make_denotation("["), den);
	    dim--;
	}
	clazz = javabind_FindClass(data_denotation(den));
	free_denotation(den, 1);
	return clazz;
    }
}
