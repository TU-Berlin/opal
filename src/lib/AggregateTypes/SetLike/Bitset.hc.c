/* hand-coded implementation part of Bitset */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/doc/LICENSE or
   http://projects.uebb.tu-berlin.de/opal/trac/wiki/License for details
   $Date$ ($Revision$)
*/

#include <unixconfig.h>

#include "Nat.oc.h"
#include "Option.oc.h"

#ifndef max
#define max(a,b) ((a) < (b) ? (b) : (a))
#endif

#ifndef min
#define min(a,b) ((a) < (b) ? (a) : (b))
#endif

#define wordsofbits(bits) ((bits+bits_bitset-1)/bits_bitset)
#define sizeofbits(bits) (wordsofbits(bits)*sizeof(BITWORD))

extern OBJ alloc_bitset(int words){
    OBJ r;
    alloc_big_flat(words,r);
    return r;
}

static int calc_words(OBJ s){	/* calculate actual used words of bitset */
    int words = words_bitset(s);
    while (words > 1 && data_bitset(s)[words-1] == 0) words--;
    return words;
}

extern OBJ dup_bitset(OBJ s, int demand){
    int currsize = calc_words(s);
    int newsize = max(demand+1,currsize);
    OBJ r = alloc_bitset(newsize);
    memcpy((void*)data_bitset(r),(void*)data_bitset(s),
		currsize*sizeof(BITWORD));
    if (currsize < newsize){
        memset((void*)(data_bitset(r)+currsize),0,
		       (newsize-currsize)*sizeof(BITWORD));
    }
    free_bitset(s,1);
    return r;
}


extern OBJ _ABitset_Sp(OBJ s1,OBJ s2) /* + */ {
    int words1 = calc_words(s1),words2 = calc_words(s2),i;
    int words = max(words1,words2);
    OBJ r;
    if (excl_bitset(s1,1) && words1 == words){
	r = s1;
    } else
    if (excl_bitset(s2,1) && words2 == words){
	r = s2;
    } else {
	r = alloc_bitset(words);
    }
    if (words1 > words2){
	int tw = words1; OBJ ts = s1;
	words1 = words2; words2 = tw;
	s1 = s2; s2 = ts;
    }
    for (i = 0; i < words1; i++){
        data_bitset(r)[i] = data_bitset(s1)[i] | data_bitset(s2)[i];
    }
    while (i < words2){
        data_bitset(r)[i] = data_bitset(s2)[i];
	i++;
    }
    if (r == s1) {
	free_bitset(s2,1);
    } else
    if (r == s2) {
	free_bitset(s1,1);
    } else {
	free_bitset(s1,1);
	free_bitset(s2,1);
    }
    return r;
}

extern OBJ _ABitset_S8(OBJ s1,OBJ s2) /* * */ {
    int words1 = calc_words(s1),words2 = calc_words(s2),i;
    int words = min(words1,words2);
    OBJ r;
    if (excl_bitset(s1,1) && words1 == words){
	r = s1;
    } else
    if (excl_bitset(s2,1) && words2 == words){
	r = s2;
    } else {
	r = alloc_bitset(words);
    }
    for (i = 0; i < words; i++){
        data_bitset(r)[i] = data_bitset(s1)[i] & data_bitset(s2)[i];
    }
    if (r == s1) {
	free_bitset(s2,1);
    } else
    if (r == s2) {
	free_bitset(s1,1);
    } else {
	free_bitset(s1,1);
	free_bitset(s2,1);
    }
    return r;
}

extern OBJ _ABitset_Sm(OBJ s1,OBJ s2) /* - */ {
    int words1 = calc_words(s1),words2 = calc_words(s2),i;
    int words = min(words1,words2);
    OBJ r;
    if (excl_bitset(s1,1)){
	r = s1;
    } else
    if (excl_bitset(s2,1) && words2 == words1){
	r = s2;
    } else {
	r = alloc_bitset(words1);
    }
    for (i = 0; i < words; i++){
        data_bitset(r)[i] = data_bitset(s1)[i] & ~(data_bitset(s2)[i]);
    }
    if (r != s1){
    	while (i < words1){
            data_bitset(r)[i] = data_bitset(s1)[i];
	    i++;
        }
    }
    if (r == s1) {
	free_bitset(s2,1);
    } else
    if (r == s2) {
	free_bitset(s1,1);
    } else {
	free_bitset(s1,1);
	free_bitset(s2,1);
    }
    return r;
}


extern OBJ _ABitset_Aarb(OBJ s) /* arb */ {
    int words = calc_words(s), i,b;
    BITWORD w, m;
    for (i = 0; i < words; i++){
	if (w = data_bitset(s)[i]){
	    for (b = 0,m = 1; ; b++, m <<= 1){
		if (w & m){
		    free_bitset(s,1);
		    return pack_nat(i * bits_bitset + b);
		}
	    }
	}
    }
    HLT("arb'Bitset: set is empty");
}


extern OBJ _ABitset_S3(OBJ s) /* # */ {
    int words = calc_words(s), i, card = 0;
    BITWORD w;
    for (i = 0; i < words; i++){
	w = data_bitset(s)[i];
	while (w){
	    if (w & 0x1) card++;
	    w >>= 1;
	}
    }
    free_bitset(s,1);
    return pack_nat(card);
}

extern OBJ _ABitset_SOC_(OBJ s) /* {}? */ {
    int words = calc_words(s), i;
    for (i = 0; i < words; i++){
	if (data_bitset(s)[i]){
    	    free_bitset(s,1);
	    return pack_clean_bool(0);
	}
    }
    free_bitset(s,1);
    return pack_clean_bool(1);
}


extern OBJ _ABitset_Aexist_(OBJ p,OBJ s) /* exist? */ {
    int words = words_bitset(s), i,b,m;
    BITWORD w;
    for (i = 0; i < words; i++){
        if (w = data_bitset(s)[i]){
            for (b = 0, m = 1; b < bits_bitset; b++, m <<= 1){
                if (w & m){
                    CPCLS(p,1);
                    if (unpack_bool( (*(OBJ (*)(OBJ,OBJ))METHOD(p,1))
                                       (p,pack_nat(i*bits_bitset + b)))){
                        FRCLS(p,1);
                        free_bitset(s,1);
                        return pack_clean_bool(1);
                    }
                }
            }
        }
    }
    FRCLS(p,1);
    free_bitset(s,1);
    return pack_clean_bool(0);
}

extern OBJ _ABitset_Afind_(OBJ p,OBJ s) /* find? */ {
    int words = words_bitset(s), i,b,m;
    BITWORD w;
    for (i = 0; i < words; i++){
        if (w = data_bitset(s)[i]){
            for (b = 0, m = 1; b < bits_bitset; b++, m <<= 1){
                if (w & m){
                    CPCLS(p,1);
                    if (unpack_bool( (*(OBJ (*)(OBJ,OBJ))METHOD(p,1))
                                       (p,pack_nat(i*bits_bitset + b)))){
                        FRCLS(p,1);
                        free_bitset(s,1);
                        return _AOption_Aavail(pack_nat(i*bits_bitset + b));
                    }
                }
            }
        }
    }
    FRCLS(p,1);
    free_bitset(s,1);
    return __AOption_Anil;
}


extern OBJ _ABitset_Sle(OBJ s1,OBJ s2) /* <= */ {
    int words1 = calc_words(s1), words2 = calc_words(s2), i;
    if (words1 > words2) goto false;
    for (i = 0; i < words1; i++){
	if ((data_bitset(s1)[i] & data_bitset(s2)[i]) != data_bitset(s1)[i]){
	    goto false;
	}
    }
    free_bitset(s1,1); free_bitset(s2,1);
    return pack_clean_bool(1);
false:
    free_bitset(s1,1); free_bitset(s2,1);
    return pack_clean_bool(0);
}

extern OBJ _ABitset_Sl(OBJ s1,OBJ s2) /* < */ {
    int words1 = calc_words(s1), words2 = calc_words(s2), i;
    if (words1 > words2) goto false;
    for (i = 0; i < words1; i++){
	BITWORD w = data_bitset(s1)[i] & data_bitset(s2)[i];
	if (w != data_bitset(s1)[i] || w == data_bitset(s2)[i] ){
	    goto false;
	}
    }
    free_bitset(s1,1); free_bitset(s2,1);
    return pack_clean_bool(1);
false:
    free_bitset(s1,1); free_bitset(s2,1);
    return pack_clean_bool(0);
}

extern OBJ _ABitset_Se(OBJ s1,OBJ s2) /* = */ {
    int words1 = calc_words(s1), words2 = calc_words(s2);
    OBJ r;
    r = pack_bool( words1 == words2 &&
		   memcmp((void*)data_bitset(s1),(void*)data_bitset(s2),
			  	words1*sizeof(BITWORD)) == 0 );
    free_bitset(s1,1); free_bitset(s2,1);
    return r;
}


extern OBJ _ABitset_SOlC(OBJ s1,OBJ s2) /* {<} */ {
    int words1 = calc_words(s1), words2 = calc_words(s2);
    OBJ r;
    r = pack_bool( words1 <= words2 &&
		   memcmp((void*)data_bitset(s1),(void*)data_bitset(s2),
			  words1*sizeof(BITWORD)) < 0 );
    free_bitset(s1,1); free_bitset(s2,1);
    return r;
}

extern OBJ _ABitset_Aincl(OBJ x2,OBJ x3) /* incl */
{OBJ r;
 ABitset_Aincl(x2,x3,r);
 return r;}

extern OBJ _ABitset_Aexcl(OBJ x2,OBJ x3) /* excl */
{OBJ r;
 ABitset_Aexcl(x2,x3,r);
 return r;}

extern OBJ _ABitset_Ain(OBJ x2,OBJ x3) /* in */
{OBJ r;
 ABitset_Ain(x2,x3,r);
 return r;}

static init_const_ABitset(){
    __ABitset_SOC = alloc_bitset(1);
    data_bitset(__ABitset_SOC)[0] = 0;
}
