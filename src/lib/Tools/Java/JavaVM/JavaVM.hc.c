/* hand-coded implementation part of JavaVM */
/* coding scheme version acc-2.1 */

#include "unixconfig.h"
#include "Array.h"
#include "Denotation.h"

#include <stdio.h>

#define DEBUG(stm) 

/* ---------------------------------------------------------------------- */
/* Variables */

JNIEnv *javabind_env;
jthrowable javabind_last_exception;
OBJ javabind_exception_ans;

static JavaVM *jvm;

extern jclass javabind_class_java_lang_String;
extern jclass javabind_class_java_lang_Object;
extern jclass javabind_class_java_lang_Class;
jmethodID javabind_equals_method;
static jmethodID javabind_getName_method;


/* ---------------------------------------------------------------------- */
/* Forward declarations */

static jclass array_type(int dim, jclass);


/* ---------------------------------------------------------------------- */
/* Initialization */

#define MEGABYTE (1024*1024)

static void vmExits(jint code){
    exit(code);
}

static void vmAborts(){
    abort();
}

static init_const_AJavaVM(){
    JDK1_1InitArgs vm_args;
    jint res;
    JNI_GetDefaultJavaVMInitArgs(&vm_args);
    vm_args.version = 0x00010001;
    vm_args.classpath = getenv("CLASSPATH");
    vm_args.minHeapSize = 2 * MEGABYTE; 
    vm_args.maxHeapSize = 48 * MEGABYTE; 
    /* vm_args.nativeStackSize = 4 * MEGABYTE; */
    /* vm_args.javaStackSize = 2 * MEGABYTE; */
    vm_args.checkSource = 0;
    vm_args.verifyMode = 0;
    vm_args.exit = vmExits;
    vm_args.abort = vmAborts;
    vm_args.enableClassGC = 1;
    vm_args.disableAsyncGC = 0;
    vm_args.enableVerboseGC = 0;
    res = JNI_CreateJavaVM(&jvm,&javabind_env,&vm_args);
    if (res < 0) {
        HLT("Can't create Java VM");
    }
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
#ifdef JAVABIND_GLOBALREFS
      (*javabind_env)->DeleteGlobalRef(javabind_env, jo); 
#else
      (*javabind_env)->DeleteLocalRef(javabind_env, jo);
#endif
    }
    dispose_foreign_mem(o,jobject_size);
}



/* ---------------------------------------------------------------------- */
/* JNI Call Abstractions */

extern jclass javabind_FindClass(const char *name){
    jclass c;
    c = (*javabind_env)->FindClass(javabind_env, name);
    javabind_catch_abort();
    if (c == NULL){
        fprintf(stderr, "JNI FindClass returns NULL for class `%s'\n", name);
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
    return res;\
  }\
  extern void javabind_Set##QUAL##Field(jobject ob, jfieldID id, TYPE x){\
    (*javabind_env)->Set##QUAL##Field(javabind_env, ob, id, x);\
  }\
  extern TYPE javabind_GetStatic##QUAL##Field(jclass clazz, jfieldID id){\
    TYPE res = (*javabind_env)->GetStatic##QUAL##Field(javabind_env,\
						      clazz, id);\
    return res;\
  }\
  extern void javabind_SetStatic##QUAL##Field(jclass clazz, jfieldID id, \
					     TYPE x){\
    (*javabind_env)->SetStatic##QUAL##Field(javabind_env, clazz, id, x);\
  }\
  extern TYPE javabind_Call##QUAL##MethodA(jobject ob, jmethodID id, \
					  jvalue argv[]){\
    TYPE res = (*javabind_env)->Call##QUAL##MethodA(javabind_env, ob, \
						   id, argv);\
    return res;\
  }\
  extern TYPE javabind_CallStatic##QUAL##MethodA(jclass clazz, jmethodID id, \
						jvalue argv[]){\
    TYPE res = (*javabind_env)->CallStatic##QUAL##MethodA(javabind_env, \
							 clazz, id, argv);\
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
    return res;
}

extern void javabind_CallVoidMethodA(jobject ob, jmethodID id, jvalue argv[]){
    (*javabind_env)->CallVoidMethodA(javabind_env, ob, id, argv);
}

extern void javabind_CallStaticVoidMethodA(jclass clazz, jmethodID id, 
					  jvalue argv[]){
    (*javabind_env)->CallStaticVoidMethodA(javabind_env, clazz, id, argv);
}

		     

/* ---------------------------------------------------------------------- */
/* Exception Handling */

extern OBJ _javabind_fail(){
    /* DEBUG */
    /* (*javabind_env)->ExceptionDescribe(javabind_env); */
    if (javabind_last_exception != NULL){
	(*javabind_env)->DeleteLocalRef(javabind_env,javabind_last_exception);
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
      memcpy(data_denotation(str), bytes, l);
      (*javabind_env)->ReleaseStringUTFChars(javabind_env, jstr, bytes);
      (*javabind_env)->DeleteLocalRef(javabind_env, jstr); 
      return str;
    } else {
      str = make_denotation("");
      set_sflag(str, null_sflag);
      return str;
    }
}

/* ---------------------------------------------------------------------- */
/* Object Conversions */

extern jobject _javabind_asObject(OBJ x){
  jobject j;
  javabind_asObject(x, j);
  return j;
}


extern OBJ _javabind_fromObject(jobject j){
  OBJ x;
  javabind_fromObject(j, x);
  return x;
}


/* ---------------------------------------------------------------------- */
/* Array Conversions */

extern jobject _javabind_asObjectArray(int dim, jclass clazz, 
				      jobject (*asJava)(OBJ),
				      OBJ arr){

  jsize leng,i, excl; jarray jarr; 
  excl = excl_array(arr, 1); 
  leng = leng_array(arr);
  jarr = (*javabind_env)->NewObjectArray(javabind_env, leng,
					 array_type(dim-1, clazz),
					 NULL);
  javabind_catch_abort();
  if (dim > 1){
    for (i = 0; i < leng; i++){
      OBJ subArr = data_array(arr)[i];
      jobject jo;
      if (!excl) copy_array(subArr, 1);
      jo = _javabind_asObjectArray(dim-1, clazz, asJava, subArr);
      (*javabind_env)->SetObjectArrayElement(javabind_env, jarr, i, jo);
      javabind_catch_abort();
      (*javabind_env)->DeleteLocalRef(javabind_env, jo);
    }
  } else {
    /* passing asJava alone doesnt works anymore since 1.1b (the free method
       is required as well). Yet, the only unregular treatment of free
       currently is for _javabind_asObject; so a quick hack
       checking for the function address helps out. */
    if (asJava == _javabind_asObject){
      for (i = 0; i < leng; i++){
	OBJ elem = data_array(arr)[i];
	(*javabind_env)->SetObjectArrayElement(javabind_env, jarr, i,
					       get_jobject(elem));
	javabind_catch_abort();
	if (excl){
	  if (excl_jobject(elem, 1)){
	    _javabind_dispose(elem);
	  } else {
	    decr_jobject(elem, 1);
	  }
	}
      }
    } else {
      for (i = 0; i < leng; i++){
	OBJ elem = data_array(arr)[i];
	jobject jo;
	if (!excl) copy_some(elem, 1);
	jo = (*asJava)(elem);
	(*javabind_env)->SetObjectArrayElement(javabind_env, jarr, i, jo);
	javabind_catch_abort();
	(*javabind_env)->DeleteLocalRef(javabind_env, jo);
      }
    }
  }
  if (excl){
    dispose_array_flat(arr);
  } else {
    decr_array(arr,1);
  }
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
   (*javabind_env)->DeleteLocalRef(javabind_env, jarr);\
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
  javabind_free_arg(x1);
  javabind_free_arg(x2);
  javabind_immutable_return(y);
}

/* ---------------------------------------------------------------------- */
/* Retrieving array element type */


static jclass array_type(int dim, jclass clazz){
    if (dim == 0){
	return clazz;
    } else {
        /* FIXME: this is a bit inefficient -- buts its only effective for
	   nested arrays. */
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

/* ---------------------------------------------------------------------- */
/* OPAL functions */

static int count_fl(OBJ l){
  int c = 0;
  while (l != NULL){
    c++; l = _getLink(_header(l));
  }
  return c;
}

extern OBJ _AJavaVM_AGcOpal(OBJ dummy){
  int i;
  _forceAllNoneFlats();
  _forceAllBigs();
  /*
  for (i = 0; i < flat_offset_ssize; i++){
    if (_freeList[i] != NULL){
      fprintf(stderr, "non-flat #%d ct %d\n", i, count_fl(_freeList[i]));
    }
  }
  for (i = flat_offset_ssize; i < 2*flat_offset_ssize; i++){
    if (_freeList[i] != NULL){
      fprintf(stderr, "flat #%d ct %d\n", i-flat_offset_ssize, 
	      count_fl(_freeList[i]));
    }
  }
  */
  return_okay_nil;
}

  
  

