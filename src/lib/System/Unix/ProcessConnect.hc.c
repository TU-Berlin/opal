/* hand-coded implementation part of ProcessConnect */
/* coding scheme version acc-2.1 */

/* Copyright 1989 - 1998 by the Opal Group, TU Berlin. All rights reserved 
   See OCSHOME/etc/LICENSE or 
   http://uebb.cs.tu-berlin.de/~opal/LICENSE.html for details
   $Date$ ($Revision$)
*/
#include "unixconfig.h"
#include <fcntl.h>
#include <signal.h>

#include "Com.h"
#include "UnixFailures.h"
#include "File.h"

static int mkFifo(char * name){
    int res;
#ifndef HAVE_MKFIFO
    char buf[1024];
    sprintf(buf, "mkfifo %s", name);
    res = system(buf);
#else
    res = mkfifo(name, 0666);
#endif
    return !res || errno == EEXIST; 
}

extern OBJ _AProcessConnect_AcOpen(OBJ InName, OBJ OutName, OBJ Swap, OBJ Void) {
    CHANNEL chan;
    int res, in, out;
    FILE * inf, * outf;

    if (!mkFifo(data_denotation(InName))) {
	return_unix_failure(errno);
    }

    if (!mkFifo(data_denotation(OutName))) {
	return_unix_failure(errno);
    }

    if (unpack_bool(Swap)){
        out = open(data_denotation(OutName), O_WRONLY); /* FIXME O_TRUNC, O_SYNC? */
        if (out != -1) {
            in = open(data_denotation(InName), O_RDONLY);
	}
    } else {
        in = open(data_denotation(InName), O_RDONLY);
        if (in != -1) {
	    out = open(data_denotation(OutName), O_WRONLY); /* FIXME O_TRUNC, O_SYNC? */
	}
    }
    if (in == -1 || out == -1) {
        int err = errno;
	if (in != -1) close(in);
	if (out != -1) close(out);
	free_denotation(InName, 1);
	free_denotation(OutName, 1);
	return_unix_failure(err);
    }
    inf = fdopen(in, "r");
    if (!inf){
	close(in);
	close(out);
	free_denotation(InName, 1);
	free_denotation(OutName, 1);
	return_unix_failure(errno);
    }
    outf = fdopen(out, "w");
    if (!outf){
	close(in);
	close(out);
	fclose(inf);
	free_denotation(InName, 1);
	free_denotation(OutName, 1);
	return_unix_failure(errno);
    }
    setbuf(inf, NULL);
    setbuf(outf, NULL);
    chan = (CHANNEL)malloc_aux(sizeof(struct sCHANNEL));
    chan->inName = InName;
    chan->outName = OutName;
    chan->in = in;
    chan->out = out;
    chan->inFile = (void*)inf;
    chan->outFile = (void*)outf;
    return_okay(pack_channel(chan));
}

extern OBJ _AProcessConnect_AcClose(OBJ Chan,OBJ Void) {
    CHANNEL chan = unpack_channel(Chan);
    fclose((FILE*)chan->inFile);
    fclose((FILE*)chan->outFile);
    close(chan->in);       /* FIXME: really necessary?? */
    close(chan->out);
    unlink(data_denotation(chan->inName));
    unlink(data_denotation(chan->outName));
    free_denotation(chan->inName,1);
    free_denotation(chan->outName,1);
    chan->inName = chan->outName = NULL;
        /* we have to keep the channel as a zombie */
    return_okay_nil;
}
    
int pconn_test_incoming(OBJ Chan){
    CHANNEL chan = unpack_channel(Chan);
    if (!chan->inName){
	return 0;
    } else {
	fd_set rfds;
	struct timeval tv;
	FD_ZERO(&rfds);
	FD_SET(chan->in, &rfds);
	tv.tv_sec = 0; 
	tv.tv_usec = 0;
	return select(chan->in+1, &rfds, NULL, NULL, &tv);
    }
}

extern OBJ _AProcessConnect_AcIncoming_(OBJ Chan, OBJ Void) {
    int resval = pconn_test_incoming(Chan);
    if (resval >= 0){
    	return_okay(pack_bool(resval));
    } else {
	return_unix_failure(errno);
    }
}

extern OBJ _AProcessConnect_AcWaitForRead(OBJ Chan, OBJ Void) {
    CHANNEL chan = unpack_channel(Chan);
    fd_set rfds;
    int res;
    FD_ZERO(&rfds);
    FD_SET(chan->in, &rfds);
    res = select(chan->in+1, &rfds, NULL, NULL, NULL);
    if (res >= 0){
    	return_okay_nil;
    } else {
    	return_unix_failure(errno);
    }
}

extern OBJ _AProcessConnect_AcWaitForWrite(OBJ Chan, OBJ Void) {
    CHANNEL chan = unpack_channel(Chan);
    fd_set wfds;
    int res;
    FD_ZERO(&wfds);
    FD_SET(chan->out, &wfds);
    res = select(chan->out+1, NULL, &wfds, NULL, NULL);
    if (res >= 0){
    	return_okay_nil;
    } else {
    	return_unix_failure(errno);
    }
}

extern OBJ _AProcessConnect_AinFile(OBJ Chan) {
    CHANNEL chan = unpack_channel(Chan);
    if (!chan->inName){
	HLT("inFile'ProcessChannel: channel closed");
    }
    return pack_file(chan->inFile);
}

extern OBJ _AProcessConnect_AoutFile(OBJ Chan) {
    CHANNEL chan = unpack_channel(Chan);
    if (!chan->outFile){
	HLT("outFile'ProcessChannel: channel closed");
    }
    return pack_file(chan->outFile);
}


static init_const_AProcessConnect(){
    init_ACom();
    init_AUnixFailures();
    init_AFile();
}
