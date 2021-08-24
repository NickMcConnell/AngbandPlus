/*
 * File: mon-desc.h
 * Purpose: Monster description
 */

#ifndef MONSTER_DESC_H
#define MONSTER_DESC_H

/*
 * Bit flags for the "monster_desc" function
 */
#define MDESC_DEFAULT   0x00    /* "it" or "the kobold" */
#define MDESC_OBJE      0x01    /* Objective (or Reflexive) */
#define MDESC_POSS      0x02    /* Possessive (or Reflexive) */
#define MDESC_IND_HID   0x04    /* Indefinites for hidden monsters */
#define MDESC_IND_VIS   0x08    /* Indefinites for visible monsters */
#define MDESC_PRO_HID   0x10    /* Pronominalize hidden monsters */
#define MDESC_PRO_VIS   0x20    /* Pronominalize visible monsters */
#define MDESC_HIDE      0x40    /* Assume the monster is hidden */
#define MDESC_SHOW      0x80    /* Assume the monster is visible */
#define MDESC_CAPITAL   0x100   /* Capitalise */

/* "someone", "something", or "the kobold" at the start of a message */
#define MDESC_STANDARD  (MDESC_CAPITAL | MDESC_IND_HID | MDESC_PRO_HID)

/* Reveal the full, indefinite name of a monster */
#define MDESC_DIED_FROM (MDESC_SHOW | MDESC_IND_VIS)

extern void plural_aux(char *name, size_t max);
extern void get_mon_name(char *buf, size_t buflen, const struct monster_race *race, int num);
extern void monster_desc(struct player *p, char *desc, size_t max, const struct monster *mon,
    int mode);

#endif /* MONSTER_DESC_H */
