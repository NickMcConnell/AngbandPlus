#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <tcl.h>
#include "dbwin.h"

void db_dump_allocs(void);
int db_heap_error(void* vp,int head);

#define _DB_BIGPAD_

#define FROMPTR(ptr) (dbst*)(((char*)ptr)-sizeof(dbst)+sizeof(int)-HMEMPAD)
#define DBSZ(siz) (sizeof(dbst)-1+siz+HMEMPAD+TMEMPAD)
#define DB_MAGIC 0xbeafdead
#define DB_MEMFILL (unsigned char)0x253
#ifdef _DB_BIGPAD_
#define _db_padbig "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
#else
#define _db_padbig
#endif
static char _db_padhead[]=_db_padbig "PaDhEaD\201";
static char _db_padtail[]="\201PaDtAiL" _db_padbig;
#define HMEMPAD (sizeof(_db_padhead)-1)
#define TMEMPAD (sizeof(_db_padtail)-1)

typedef struct dbst {
  int magic;
  struct dbst* next, *prev;
  unsigned int siz;
  int line;
  unsigned int mark:31;
  unsigned int dead:1;
  char *file;
  char d[1];
}  dbst;

int db_mark=0;
int db_fatal=0;
int db_interval=1;
int db_verbose=0;
int db_quiet=0;
int db_nofree=0;
int db_freebad=0;
int db_writes=1;
int db_checkstart=0;  /* Start all heap checks from this mark */

static dbst *heap=0;
static dbst *freeheap=0;
static int db_is_init=0;
static int db_zero=1;
static int db_atexit=1;
static int hcnt=0;
static int db_heap=0;

static int objcmd_dbcheck(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	db_dump_allocs();
	return TCL_OK;
}

static void _db_init(void) {
    char *cp,  *e=getenv("DB_ALLOC");
    db_is_init=1;
    if (e) {
	while (e && isalpha(*e)) {
	  if (*(cp=(e+1)) != '=')  {
	     dbwin("Option error at %s\n%s", e);
	     exit(-1);
	  }
	  db_heap=1;
	  switch (e[0]) {
	    case 'd': break;
	    case 'z': db_zero=atoi(cp+1); break;
	    case 'x': db_atexit=atoi(cp+1); break;
	    case 'f': db_fatal=atoi(cp+1); break;
	    case 'i': db_interval=atoi(cp+1); break;
	    case 'v': db_verbose=atoi(cp+1); break;
	    case 'q': db_quiet=atoi(cp+1); break;
	    case 'n': db_nofree=atoi(cp+1); break;
	    case 'b': db_freebad=atoi(cp+1); break;
#if 0
	    case 'l': {
	    	char file[BUFSIZ], *ecp;
		strncpy(file,cp+1,BUFSIZ-1);
	  	if (ecp=strchr(cp+1,',')) file[ecp-cp]=0;
    	  	if (!(logfile=fopen(file,"w"))) {
		  dbwin("open failed %s\n", file);
		  exit(-1);
		}
		}
	    	break;
#endif
	    default:
	    case 'w':
	    	if (e[0]=='w' && db_nofree) { db_writes=atoi(cp+1); break; }
	        dbwin("Unknown option: %c, %s\n%s", e[0], e);
		exit(-1);
	  }
	  if ((e=strchr(cp,','))) e++;
	}
    }
   if (db_heap && db_atexit)
      atexit(db_dump_allocs);

{
	extern Tcl_Interp *TclTk_Interp(void);
	Tcl_Interp *interp = TclTk_Interp();
	Tcl_CreateObjCommand(interp, "dbcheck", objcmd_dbcheck, NULL, NULL);
}
}

static void db_fromstr(dbst *v) {
    if (v->file[0])
	dbwin("from line %d in file %s\n", v->line, v->file);
    else
	dbwin("from addr 0x%x\n",v->line);
}

static void _db_heap_check_v(void *vp) {
    dbst *v=vp;
    if (v->dead) {
      if (!db_quiet) {
        dbwin("%x(%d): used dead pointer ", v,v->mark);
	db_fromstr(v);
      }
      return;
    }
    if (memcmp(v->d,_db_padhead,HMEMPAD))
       if (db_heap_error(v,1) && db_fatal) abort();
    if (memcmp(v->d+HMEMPAD+v->siz,_db_padtail,TMEMPAD))
       if (db_heap_error(v,0) && db_fatal) abort();
}

static void _db_free_check_v(dbst *v) {
    int i,j,k;
    for (i=v->siz+HMEMPAD+TMEMPAD-1; i>=0; i--)
      if (v->d[i] != DB_MEMFILL) {
        dbwin("%x(%d) freed memory written to: ", v, v->mark);
        j=i;
        while (j>0 && v->d[j-1] != DB_MEMFILL) j--;
        for (k=j; k<=i; k++)
               if (isprint(v->d[k]))
       	  dbwin("%c",v->d[k]);
    	else
    	  dbwin("<%x>",v->d[k]);
        dbwin(" :%d bytes at offset %d in size %d ",
		i-j+1, j-HMEMPAD, v->siz);
	db_fromstr(v);
      }
}

static void db_free_check(int mark) {
    dbst *v=freeheap;
    if (!db_writes) return;
    while (v) {
      if (v->mark >= mark)
      	 _db_free_check_v(v);
       v=v->next;
    }
}

  
void db_heap_check(int mark) {
    dbst* v;
    if (!db_is_init) _db_init();
    for (v=heap; v; v=v->next)
	if (v->mark >= mark)
	  _db_heap_check_v(v);
}

void db_dump_since(int mark) {
    int hdr=0;
    dbst* v;
    for (v=heap; v; v=v->next)
	if (v->mark>=mark) {
          if (!hdr) {
	     dbwin("UNFREED ALLOCATIONS:\n");
	     hdr=1;
	  }
	  _db_heap_check_v(v);
	  dbwin("%x(%d): %d bytes allocated ", v,v->mark,v->siz);
	  db_fromstr(v);
        }
}

/* Dump all allocations */
void db_dump_allocs(void) {
   db_dump_since(db_checkstart);
   db_free_check(db_checkstart);
}

static char * _db_mempad(dbst *v, unsigned siz) {
   v->magic=DB_MAGIC;
   memcpy(v->d,_db_padhead,HMEMPAD);
   memcpy(v->d+HMEMPAD+siz,_db_padtail,TMEMPAD);
   v->siz=siz;
   if (db_interval && (hcnt++ >= db_interval)) {
     db_heap_check(db_checkstart);
     db_free_check(db_checkstart);
     hcnt=0;
   }
   return v->d+HMEMPAD;
}

#define DBSETUP v=DBSetup(siz,file,line);

dbst* DBSetup(int siz, char* file, int line) {
  dbst *v=(dbst*)Tcl_Alloc(DBSZ(siz));
  memset(v, 0, sizeof(dbst)); /* TNB */
  v->file=file;
  v->line=line;
  v->mark=++db_mark;
  if (db_verbose) {
    dbwin("%x(%d): allocated %d bytes ", v, v->mark, siz);
    db_fromstr(v);
  }
  if (!heap) return heap=v;
  v->next=heap; 
  heap->prev=v;
  return heap=v;
}

DLLEXPORT
void *_db_malloc(size_t siz, char *file, int line) {
   dbst *v;
   if (!db_is_init) _db_init();
   if (!db_heap) return Tcl_Alloc(siz);
   DBSETUP
   /* printf("%x\n", v); */
   return _db_mempad(v,siz);
}

void *_db_calloc(size_t sz, size_t cnt, char *file, int line) {
   char *p;
   unsigned int siz;
   dbst *v;
   if (!db_is_init) _db_init();
   if (!db_heap) return Tcl_Alloc(sz * cnt);
   siz=sz*cnt;
   DBSETUP
   p=_db_mempad(v,siz);
   memset(p,0,siz);
   return p;
}

DLLEXPORT
void *_db_realloc(void *ptr, size_t siz, char *file, int line) {
   dbst *nv, *pv, *v,*ov;
   if (!db_is_init) _db_init();
   if (!db_heap) return Tcl_Realloc(ptr, siz);
#if 1 /* July 18 2004 */
   if (ptr == NULL) return _db_malloc(siz, file, line);
#endif
   ov=v=FROMPTR(ptr);
   if (v->magic!=DB_MAGIC) {
       if (!db_quiet)
         dbwin("%x: magic failed %d from line %d of %s\n",
		v, siz,line,file);
	if (db_heap_error(v,3))
	    abort();
	if (db_freebad) return Tcl_Realloc(ptr, siz);
	return ptr;
   }
   if (siz<v->siz) return ptr;
   pv=v->prev; nv=v->next;
   v->line=line;
   v->file=file;
#if 1 /* Aug 14 2004 TNB */
   v=(dbst *)Tcl_Realloc((char *) v,DBSZ(siz));
#else
   v=(dbst *)Tcl_Realloc((char *) v,siz+HMEMPAD+TMEMPAD+sizeof(dbst));
#endif
   if (db_verbose) {
     dbwin("%x(%d) realloced %d -> %d bytes from line %d of %s\n", v,
	v->mark, v->siz, siz, line, file);
   }
   v->siz = siz; /* TNB */
   memcpy(v->d+HMEMPAD+siz,_db_padtail,TMEMPAD);
   if (ov==v) return ptr;
   if (ov==heap) heap=v;
   if (pv) pv->next=v;
   if (nv) nv->prev=v;
   return v->d+HMEMPAD;
}

char *_db_strdup(const char *str, char *file, int line) {
   char *p;
   unsigned int siz;
   dbst *v;
   if (!db_is_init) _db_init();
   siz=strlen(str)+1;
   if (!db_heap) {
      p=Tcl_Alloc(siz);
   } else {
     DBSETUP
     p=_db_mempad(v,siz);
   }
   strcpy(p,str);
   return p;
}

DLLEXPORT
void _db_free(void *ptr, char *file, int line) {
   dbst *v;
   if (!db_is_init) _db_init();
   if (!db_heap) { if (ptr) Tcl_Free(ptr); return; }
   if (!ptr) {
      if (!db_quiet) dbwin("free null from line %d of %s\n", line, file);
      if (db_zero)
        db_heap_error(0,2);
      return;
   }
   v=FROMPTR(ptr);
   if (v->magic!=DB_MAGIC) {
        if (!db_quiet)
          dbwin("%x: magic check failed in free from line %d of %s\n", ptr, line, file);
	if (db_heap_error(v,3))
	    abort();
	if ((!db_nofree) && db_freebad) Tcl_Free(ptr);
	return;
   }
   if (!v->dead)
      _db_heap_check_v(v);
   else if (!db_quiet) {
        dbwin("%x(%d): free dead pointer ", v,v->mark);
	db_fromstr(v);
   }
   v->dead=1;
   if (v==heap) heap=v->next;
   else {
     if (!v->prev) {
        dbwin("v->prev is NULL\n");
        Tcl_Panic("_db_free: v->prev is NULL");
     }
     if ((v->prev->next=v->next))
        v->next->prev=v->prev;
   }
   if (db_verbose)
	dbwin("%x(%d) freeing\n", v, v->mark);
   if (!db_nofree) Tcl_Free((char *) v);
   else {  /* Save on freeheap to check for later writes. */
     memset(v->d,DB_MEMFILL,v->siz+HMEMPAD+TMEMPAD);
     if (!freeheap) {
        v->next=0;
	freeheap=v;
     } else {
	v->next=freeheap;
	freeheap=v;
     }
   }
}

static char *paddump(char *buf, char *str, int pad) {
  int i, j;
  for (i=0, j=0; i<pad; i++, j+=2)
    sprintf(buf+j,"%.2x",str[i]);
  sprintf(buf+j," : %.*s", pad, str);
  return buf;
}

int db_heap_error(void* vp,int head) {
    dbst *v=vp;
    int rc=db_fatal;
    char hbuf[HMEMPAD*10];
    char tbuf[TMEMPAD*10];
    char dbuf[100];
    if (head>1) return rc;
    dbwin("%x(%d): %s fence post overwrite, alloced from ",v,v->mark,head?"Head":"Tail");
    if (v->file[0])
      dbwin("line %d in file %s",v->line,v->file);
    else
      dbwin("addr 0x%x",v->line);
    dbwin("\n\tBefore = %s\n\tAfter  = %s\n\tData   = %s\n", 
	paddump(hbuf,head?_db_padhead:_db_padtail,HMEMPAD), 
	paddump(tbuf,head?v->d:v->d+v->siz+HMEMPAD, TMEMPAD),
        paddump(dbuf,v->d+HMEMPAD,(v->siz<60?v->siz:60)));
    return rc;
}

