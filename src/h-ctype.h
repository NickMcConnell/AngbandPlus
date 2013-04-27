/*** Character tests ***/

#define CTYPE_VOWEL		 0x01

#define is_vowel(C) \
	((char_test[C + 128] & CTYPE_VOWEL) != 0)

