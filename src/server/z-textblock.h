/*
 * File: z-textblock.h
 * Purpose: Text output bugger code
 */

#ifndef INCLUDED_Z_TEXTBLOCK_H
#define INCLUDED_Z_TEXTBLOCK_H

typedef void (*text_writer)(struct player *p, ang_file *f);

extern void text_out_init(struct player *p);
extern void text_out(struct player *p, const char *fmt, ...);
extern void text_out_c(struct player *p, byte a, const char *fmt, ...);
extern void text_out_done(struct player *p);
extern void text_out_done_no_newline(struct player *p);
extern errr text_lines_to_file(struct player *p, const char *path, text_writer writer);

#endif /* INCLUDED_Z_TEXTBLOCK_H */
