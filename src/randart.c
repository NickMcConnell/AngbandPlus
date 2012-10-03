/* File: randart.c */


/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#include "init.h"

/*
 * Random artifact generator (randart) by Greg Wooledge.
 *
 * The external "names.txt" file was sucked into this file for simplicity.
 */

#ifdef GJW_RANDART

int last_power = 0;
int names_list_len = 0;
/*bool art_cursed = FALSE;*/
char* new_art_names = 0;

static cptr names_list =
"adanedhel\n"
"adurant\n"
"aeglos\n"
"aegnor\n"
"aelin\n"
"aeluin\n"
"aerandir\n"
"aerin\n"
"agarwaen\n"
"aglareb\n"
"aglarond\n"
"aglon\n"
"ainulindale\n"
"ainur\n"
"alcarinque\n"
"aldaron\n"
"aldudenie\n"
"almaren\n"
"alqualonde\n"
"aman\n"
"amandil\n"
"amarie\n"
"amarth\n"
"amlach\n"
"amon\n"
"amras\n"
"amrod\n"
"anach\n"
"anar\n"
"anarion\n"
"ancalagon\n"
"ancalimon\n"
"anarrima\n"
"andor\n"
"andram\n"
"androth\n"
"anduin\n"
"andunie\n"
"anfauglir\n"
"anfauglith\n"
"angainor\n"
"angband\n"
"anghabar\n"
"anglachel\n"
"angrenost\n"
"angrim\n"
"angrist\n"
"angrod\n"
"anguirel\n"
"annael\n"
"annatar\n"
"annon\n"
"annuminas\n"
"apanonar\n"
"aradan\n"
"aragorn\n"
"araman\n"
"aranel\n"
"aranruth\n"
"aranwe\n"
"aras\n"
"aratan\n"
"aratar\n"
"arathorn\n"
"arda\n"
"ard-galen\n"
"aredhel\n"
"ar-feiniel\n"
"argonath\n"
"arien\n"
"armenelos\n"
"arminas\n"
"arnor\n"
"aros\n"
"arossiach\n"
"arthad\n"
"arvernien\n"
"arwen\n"
"ascar\n"
"astaldo\n"
"atalante\n"
"atanamir\n"
"atanatari\n"
"atani\n"
"aule\n"
"avallone\n"
"avari\n"
"avathar\n"
"balan\n"
"balar\n"
"balrog\n"
"barad\n"
"baragund\n"
"barahir\n"
"baran\n"
"baranduin\n"
"bar\n"
"bauglir\n"
"beleg\n"
"belegaer\n"
"belegost\n"
"belegund\n"
"beleriand\n"
"belfalas\n"
"belthil\n"
"belthronding\n"
"beor\n"
"beraid\n"
"bereg\n"
"beren\n"
"boromir\n"
"boron\n"
"bragollach\n"
"brandir\n"
"bregolas\n"
"bregor\n"
"brethil\n"
"brilthor\n"
"brithiach\n"
"brithombar\n"
"brithon\n"
"cabed\n"
"calacirya\n"
"calaquendi\n"
"calenardhon\n"
"calion\n"
"camlost\n"
"caragdur\n"
"caranthir\n"
"carcharoth\n"
"cardolan\n"
"carnil\n"
"celeborn\n"
"celebrant\n"
"celebrimbor\n"
"celebrindal\n"
"celebros\n"
"celegorm\n"
"celon\n"
"cirdan\n"
"cirith\n"
"cirth\n"
"ciryatan\n"
"ciryon\n"
"coimas\n"
"corollaire\n"
"crissaegrim\n"
"cuarthal\n"
"cuivienen\n"
"culurien\n"
"curufin\n"
"curufinwe\n"
"curunir\n"
"cuthalion\n"
"daedeloth\n"
"daeron\n"
"dagnir\n"
"dagor\n"
"dagorlad\n"
"dairuin\n"
"danwedh\n"
"delduwath\n"
"denethor\n"
"dimbar\n"
"dimrost\n"
"dinen\n"
"dior\n"
"dirnen\n"
"dolmed\n"
"doriath\n"
"dorlas\n"
"dorthonion\n"
"draugluin\n"
"drengist\n"
"duath\n"
"duinath\n"
"duilwen\n"
"dunedain\n"
"dungortheb\n"
"earendil\n"
"earendur\n"
"earnil\n"
"earnur\n"
"earrame\n"
"earwen\n"
"echor\n"
"echoriath\n"
"ecthelion\n"
"edain\n"
"edrahil\n"
"eglador\n"
"eglarest\n"
"eglath\n"
"eilinel\n"
"eithel\n"
"ekkaia\n"
"elbereth\n"
"eldalie\n"
"eldalieva\n"
"eldamar\n"
"eldar\n"
"eledhwen\n"
"elemmire\n"
"elende\n"
"elendil\n"
"elendur\n"
"elenna\n"
"elentari\n"
"elenwe\n"
"elerrina\n"
"elleth\n"
"elmoth\n"
"elostirion\n"
"elrond\n"
"elros\n"
"elu\n"
"eluchil\n"
"elured\n"
"elurin\n"
"elwe\n"
"elwing\n"
"emeldir\n"
"endor\n"
"engrin\n"
"engwar\n"
"eol\n"
"eonwe\n"
"ephel\n"
"erchamion\n"
"ereb\n"
"ered\n"
"erech\n"
"eregion\n"
"ereinion\n"
"erellont\n"
"eressea\n"
"eriador\n"
"eru\n"
"esgalduin\n"
"este\n"
"estel\n"
"estolad\n"
"ethir\n"
"ezellohar\n"
"faelivrin\n"
"falas\n"
"falathar\n"
"falathrim\n"
"falmari\n"
"faroth\n"
"fauglith\n"
"feanor\n"
"feanturi\n"
"felagund\n"
"finarfin\n"
"finduilas\n"
"fingolfin\n"
"fingon\n"
"finwe\n"
"firimar\n"
"formenos\n"
"fornost\n"
"frodo\n"
"fuin\n"
"fuinur\n"
"gabilgathol\n"
"galad\n"
"galadriel\n"
"galathilion\n"
"galdor\n"
"galen\n"
"galvorn\n"
"gandalf\n"
"gaurhoth\n"
"gelion\n"
"gelmir\n"
"gelydh\n"
"gil\n"
"gildor\n"
"giliath\n"
"ginglith\n"
"girith\n"
"glaurung\n"
"glingal\n"
"glirhuin\n"
"gloredhel\n"
"glorfindel\n"
"golodhrim\n"
"gondolin\n"
"gondor\n"
"gonnhirrim\n"
"gorgoroth\n"
"gorlim\n"
"gorthaur\n"
"gorthol\n"
"gothmog\n"
"guilin\n"
"guinar\n"
"guldur\n"
"gundor\n"
"gurthang\n"
"gwaith\n"
"gwareth\n"
"gwindor\n"
"hadhodrond\n"
"hador\n"
"haladin\n"
"haldad\n"
"haldan\n"
"haldar\n"
"haldir\n"
"haleth\n"
"halmir\n"
"handir\n"
"harad\n"
"hareth\n"
"hathaldir\n"
"hathol\n"
"haudh\n"
"helcar\n"
"helcaraxe\n"
"helevorn\n"
"helluin\n"
"herumor\n"
"herunumen\n"
"hildorien\n"
"himlad\n"
"himring\n"
"hirilorn\n"
"hisilome\n"
"hithaeglir\n"
"hithlum\n"
"hollin\n"
"huan\n"
"hunthor\n"
"huor\n"
"hurin\n"
"hyarmendacil\n"
"hyarmentir\n"
"iant\n"
"iaur\n"
"ibun\n"
"idril\n"
"illuin\n"
"ilmare\n"
"ilmen\n"
"iluvatar\n"
"imlach\n"
"imladris\n"
"indis\n"
"ingwe\n"
"irmo\n"
"isil\n"
"isildur\n"
"istari\n"
"ithil\n"
"ivrin\n"
"kelvar\n"
"kementari\n"
"ladros\n"
"laiquendi\n"
"lalaith\n"
"lamath\n"
"lammoth\n"
"lanthir\n"
"laurelin\n"
"leithian\n"
"legolin\n"
"lembas\n"
"lenwe\n"
"linaewen\n"
"lindon\n"
"lindorie\n"
"loeg\n"
"lomelindi\n"
"lomin\n"
"lomion\n"
"lorellin\n"
"lorien\n"
"lorindol\n"
"losgar\n"
"lothlann\n"
"lothlorien\n"
"luin\n"
"luinil\n"
"lumbar\n"
"luthien\n"
"mablung\n"
"maedhros\n"
"maeglin\n"
"maglor\n"
"magor\n"
"mahanaxar\n"
"mahtan\n"
"maiar\n"
"malduin\n"
"malinalda\n"
"mandos\n"
"manwe\n"
"mardil\n"
"melian\n"
"melkor\n"
"menegroth\n"
"meneldil\n"
"menelmacar\n"
"meneltarma\n"
"minas\n"
"minastir\n"
"mindeb\n"
"mindolluin\n"
"mindon\n"
"minyatur\n"
"mirdain\n"
"miriel\n"
"mithlond\n"
"mithrandir\n"
"mithrim\n"
"mordor\n"
"morgoth\n"
"morgul\n"
"moria\n"
"moriquendi\n"
"mormegil\n"
"morwen\n"
"nahar\n"
"naeramarth\n"
"namo\n"
"nandor\n"
"nargothrond\n"
"narog\n"
"narsil\n"
"narsilion\n"
"narya\n"
"nauglamir\n"
"naugrim\n"
"ndengin\n"
"neithan\n"
"neldoreth\n"
"nenar\n"
"nenning\n"
"nenuial\n"
"nenya\n"
"nerdanel\n"
"nessa\n"
"nevrast\n"
"nibin\n"
"nienna\n"
"nienor\n"
"nimbrethil\n"
"nimloth\n"
"nimphelos\n"
"nimrais\n"
"nimras\n"
"ningloron\n"
"niniel\n"
"ninniach\n"
"ninquelote\n"
"niphredil\n"
"nirnaeth\n"
"nivrim\n"
"noegyth\n"
"nogrod\n"
"noldolante\n"
"noldor\n"
"numenor\n"
"nurtale\n"
"obel\n"
"ohtar\n"
"oiolosse\n"
"oiomure\n"
"olorin\n"
"olvar\n"
"olwe\n"
"ondolinde\n"
"orfalch\n"
"ormal\n"
"orocarni\n"
"orodreth\n"
"orodruin\n"
"orome\n"
"oromet\n"
"orthanc\n"
"osgiliath\n"
"osse\n"
"ossiriand\n"
"palantir\n"
"pelargir\n"
"pelori\n"
"periannath\n"
"quendi\n"
"quenta\n"
"quenya\n"
"radagast\n"
"radhruin\n"
"ragnor\n"
"ramdal\n"
"rana\n"
"rathloriel\n"
"rauros\n"
"region\n"
"rerir\n"
"rhovanion\n"
"rhudaur\n"
"rhun\n"
"rhunen\n"
"rian\n"
"ringil\n"
"ringwil\n"
"romenna\n"
"rudh\n"
"rumil\n"
"saeros\n"
"salmar\n"
"saruman\n"
"sauron\n"
"serech\n"
"seregon\n"
"serinde\n"
"shelob\n"
"silmarien\n"
"silmaril\n"
"silpion\n"
"sindar\n"
"singollo\n"
"sirion\n"
"soronume\n"
"sul\n"
"sulimo\n"
"talath\n"
"taniquetil\n"
"tar\n"
"taras\n"
"tarn\n"
"tathren\n"
"taur\n"
"tauron\n"
"teiglin\n"
"telchar\n"
"telemnar\n"
"teleri\n"
"telperion\n"
"telumendil\n"
"thalion\n"
"thalos\n"
"thangorodrim\n"
"thargelion\n"
"thingol\n"
"thoronath\n"
"thorondor\n"
"thranduil\n"
"thuringwethil\n"
"tilion\n"
"tintalle\n"
"tinuviel\n"
"tirion\n"
"tirith\n"
"tol\n"
"tulkas\n"
"tumhalad\n"
"tumladen\n"
"tuna\n"
"tuor\n"
"turambar\n"
"turgon\n"
"turin\n"
"uial\n"
"uilos\n"
"uinen\n"
"ulairi\n"
"ulmo\n"
"ulumuri\n"
"umanyar\n"
"umarth\n"
"umbar\n"
"ungoliant\n"
"urthel\n"
"uruloki\n"
"utumno\n"
"vaire\n"
"valacirca\n"
"valandil\n"
"valaquenta\n"
"valar\n"
"valaraukar\n"
"valaroma\n"
"valier\n"
"valimar\n"
"valinor\n"
"valinoreva\n"
"valmar\n"
"vana\n"
"vanyar\n"
"varda\n"
"vasa\n"
"vilya\n"
"vingilot\n"
"vinyamar\n"
"voronwe\n"
"wethrin\n"
"wilwarin\n"
"yavanna\n"
;

#define MAX_TRIES 50
#define BUFLEN 1024

#define MIN_NAME_LEN 5
#define MAX_NAME_LEN 9
#define S_WORD 26
#define E_WORD S_WORD

#define sign(x)	((x) > 0 ? 1 : ((x) < 0 ? -1 : 0))


static unsigned short lprobs[S_WORD+1][S_WORD+1][S_WORD+1];
static unsigned short ltotal[S_WORD+1][S_WORD+1];

/*
 * Cache the results of lookup_kind(), which is expensive and would
 * otherwise be called much too often.
 */
static s16b *kinds = 0;

/* Global just for convenience. */
static int randart_verbose = 0;


/*
 * Use W. Sheldon Simms' random name generator.  This function builds
 * probability tables which are used later on for letter selection.  It
 * relies on the ASCII character set.
 */
static void build_prob(cptr learn)
{
	int c_prev, c_cur, c_next;

	/* Build raw frequencies */
	do
	{
		c_prev = c_cur = S_WORD;

		do
		{
			c_next = *learn++;
		} while (!isalpha(c_next) && (c_next != '\0'));

		if (c_next == '\0') break;

		do
		{
			c_next = A2I(tolower(c_next));
			lprobs[c_prev][c_cur][c_next]++;
			ltotal[c_prev][c_cur]++;
			c_prev = c_cur;
			c_cur = c_next;
			c_next = *learn++;
		} while (isalpha(c_next));

		lprobs[c_prev][c_cur][E_WORD]++;
		ltotal[c_prev][c_cur]++;
	}
	while (c_next != '\0');
}


/*
 * Use W. Sheldon Simms' random name generator.  Generate a random word using
 * the probability tables we built earlier.  Relies on the ASCII character
 * set.  Relies on European vowels (a, e, i, o, u).  The generated name should
 * be copied/used before calling this function again.
 */
static char *make_word(void)
{
	static char word_buf[90];
	int r, totalfreq;
	int tries, lnum, vow;
	int c_prev, c_cur, c_next;
	char *cp;

startover:
	vow = 0;
	lnum = 0;
	tries = 0;
	cp = word_buf;
	c_prev = c_cur = S_WORD;

	while (1)
	{
	    getletter:
		c_next = 0;
		r = rand_int(ltotal[c_prev][c_cur]);
		totalfreq = lprobs[c_prev][c_cur][c_next];

		while (totalfreq <= r)
		{
			c_next++;
			totalfreq += lprobs[c_prev][c_cur][c_next];
		}

		if (c_next == E_WORD)
		{
			if ((lnum < MIN_NAME_LEN) || vow == 0)
			{
				tries++;
				if (tries < 10) goto getletter;
				goto startover;
			}
			*cp = '\0';
			break;
		}

		if (lnum >= MAX_NAME_LEN) goto startover;

		*cp = I2A(c_next);

		if (is_a_vowel(*cp)) vow++;

		cp++;
		lnum++;
		c_prev = c_cur;
		c_cur = c_next;
	}

	word_buf[0] = toupper(word_buf[0]);

	return (word_buf);
}


/*
 * Use W. Sheldon Simms' random name generator.
 */
static errr init_names(void)
{
	char buf[BUFLEN];
	size_t name_size;
	char *a_base;
	char *a_next;
	int i;

	/* Temporary space for names, while reading and randomizing them. */
	cptr *names;


	build_prob(names_list);

	/* Allocate the "names" array */
	/* ToDo: Make sure the memory is freed correctly in case of errors */
	C_MAKE(names, z_info->a_max, cptr);

	for (i = 0; i < z_info->a_max; i++)
	{
		char *word = make_word();

		if (rand_int(3) == 0)
			sprintf(buf, "'%s'", word);
		else
			sprintf(buf, "of %s", word);

		names[i] = string_make(buf);
	}

	/* Special cases -- keep these three names separate. */
	string_free(names[ART_POWER - 1]);
	string_free(names[ART_GROND - 1]);
	string_free(names[ART_MORGOTH - 1]);
	names[ART_POWER - 1] = string_make("of Power (The One Ring)");
	names[ART_GROND - 1] = string_make("'Grond'");
	names[ART_MORGOTH - 1] = string_make("of Morgoth");

	/* Convert our names array into an a_name structure for later use. */
	name_size = 0;

	for (i = 1; i < z_info->a_max; i++)
	{
		name_size += strlen(names[i-1]) + 2;	/* skip first char */
	}

	C_MAKE(a_base, name_size, char);

	a_next = a_base + 1;	/* skip first char */

	for (i = 1; i < z_info->a_max; i++)
	{
		strcpy(a_next, names[i-1]);
		if (a_info[i].tval > 0)		/* skip unused! */
			a_info[i].name = a_next - a_base;
		a_next += strlen(names[i-1]) + 1;
	}

	/* Free the old names */
	FREE(a_name);

	for (i = 0; i < z_info->a_max; i++)
	{
		string_free(names[i]);
	}

	/* Free the "names" array */
	FREE((void*)names);

	/* Store the names */
	a_name = a_base;
	a_head.name_ptr = a_base;
	a_head.name_size = name_size;

	/* Success */
	return (0);
}


/*
 * Calculate the multiplier we'll get with a given bow type.
 */
static int bow_multiplier(int sval)
{
	switch (sval)
	{
		case SV_SLING:
		case SV_SHORT_BOW:
			return (2);
		case SV_LONG_BOW:
		case SV_LIGHT_XBOW:
			return (3);
		case SV_HEAVY_XBOW:
			return (4);
		default:
			msg_format("Illegal bow sval %s", sval);
	}

	return (0);
}


/*
 * Evaluate the artifact's overall power level.
 * Alex: a_idx = 0 if artifact does not yet added by add_artifact()
 */
s32b artifact_power(int a_idx, const artifact_type *a_ptr)
{
	s32b p = 0;
	s16b k_idx = 0;
	object_kind *k_ptr;
	int immunities = 0;

	/* Try to use the cache */
        if (a_idx)
	        k_idx = kinds[a_idx];

	/* Lookup the item if not yet cached */
	if (!k_idx)
	{
		k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* Cache the object index */
                if (a_idx)
		        kinds[a_idx] = k_idx;

		/* Paranoia */
		if (!k_idx)
		{
			quit_fmt("Illegal tval/sval value for artifact %d!", a_idx);
		}
	}

	k_ptr = &k_info[k_idx];

        /* Start with a "power" rating derived from the base item's level. */
	/*if (!is_special(a_ptr->tval))
	{
		p = (k_ptr->level + 7) / 8;
	}*/

	/* Evaluate certain abilities based on type of object. */
	switch (a_ptr->tval)
	{
		case TV_BOW:
		{
			int mult;

			p += (a_ptr->to_d + sign(a_ptr->to_d)) / 2;
			mult = bow_multiplier(a_ptr->sval);
			if (a_ptr->flags1 & TR1_MIGHT)
			{
				if (a_ptr->pval > 3)
				{
					p += 20000;	/* inhibit */
					mult = 1;	/* don't overflow */
				}
				else
					mult += a_ptr->pval;
			}
			p *= mult;

			if (a_ptr->to_d > ART_W_1) p += (a_ptr->to_d - ART_W_1 + 1) / 2;/*Alex*/
			if (a_ptr->to_d > ART_W_2) p += (a_ptr->to_d - ART_W_2 + 1) / 2;/*Alex*/
			if (a_ptr->to_d > ART_W_3) p += 20000;/*Alex*/

			if (a_ptr->flags1 & TR1_SHOTS)
			{
				if (a_ptr->pval > 3)
					p += 20000;	/* inhibit */
				else if (a_ptr->pval > 0)
					p *= (2 * a_ptr->pval);
			}

			p += (a_ptr->to_h + 3 * sign(a_ptr->to_h)) / 4;
			if (a_ptr->to_h > ART_W_1) p += (a_ptr->to_h - ART_W_1 + 1) / 2;/*Alex*/
			if (a_ptr->to_h > ART_W_2) p += (a_ptr->to_h - ART_W_2 + 1) / 2;/*Alex*/
			if (a_ptr->to_h > ART_W_3) p += 20000;/*Alex*/
			if (a_ptr->weight < k_ptr->weight) p++;
			break;
		}
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			/*p += (a_ptr->dd * a_ptr->ds + 1) / 2; - wrong*/
                        /* Alex: this is right (according to mean value) */
			p += a_ptr->dd * (a_ptr->ds + 1) / 2;
			if (a_ptr->flags1 & TR1_SLAY_EVIL) p = (p * 3) / 2;
			if (a_ptr->flags1 & TR1_KILL_DRAGON) p = (p * 3) / 2;
			if (a_ptr->flags1 & TR1_KILL_DEMON) p = (p * 3) / 2;
			if (a_ptr->flags1 & TR1_KILL_UNDEAD) p = (p * 3) / 2;
			if (a_ptr->flags1 & TR1_SLAY_ANIMAL) p = (p * 4) / 3;
			if (a_ptr->flags1 & TR1_SLAY_UNDEAD) p = (p * 4) / 3;
			if (a_ptr->flags1 & TR1_SLAY_DRAGON) p = (p * 4) / 3;
			if (a_ptr->flags1 & TR1_SLAY_DEMON) p = (p * 5) / 4;
			if (a_ptr->flags1 & TR1_SLAY_TROLL) p = (p * 6) / 5;/*Alex: was 5/4*/
			if (a_ptr->flags1 & TR1_SLAY_ORC) p = (p * 6) / 5;/*Alex: was 5/4*/
			if (a_ptr->flags1 & TR1_SLAY_GIANT) p = (p * 7) / 6;/*Alex: was 6/5*/

			if (a_ptr->flags1 & TR1_BRAND_ACID) p = p * 2;
			if (a_ptr->flags1 & TR1_BRAND_ELEC) p = (p * 3) / 2;
			if (a_ptr->flags1 & TR1_BRAND_FIRE) p = (p * 4) / 3;
			if (a_ptr->flags1 & TR1_BRAND_COLD) p = (p * 4) / 3;

			p += (a_ptr->to_d + 2 * sign(a_ptr->to_d)) / 3;
			if (a_ptr->to_d > ART_W_1) p += (a_ptr->to_d - ART_W_1 + 1) / 2;
			if (a_ptr->to_d > ART_W_2) p += (a_ptr->to_d - ART_W_2 + 1) / 2;/*Alex*/
			if (a_ptr->to_d > ART_W_3) p += 20000;/*Alex*/

			if (a_ptr->flags1 & TR1_BLOWS)
			{
				if (a_ptr->pval > 3)
					p += 20000;	/* inhibit */
				else if (a_ptr->pval > 0)
					p = (p * 6) / (4 - a_ptr->pval);
			}

			if ((a_ptr->flags1 & TR1_TUNNEL) &&
			    (a_ptr->tval != TV_DIGGING))
				p += a_ptr->pval * 3;

			p += (a_ptr->to_h + 3 * sign(a_ptr->to_h)) / 4;
			if (a_ptr->to_h > ART_W_1) p += (a_ptr->to_h - ART_W_1 + 1) / 2;/*Alex*/
			if (a_ptr->to_h > ART_W_2) p += (a_ptr->to_h - ART_W_2 + 1) / 2;/*Alex*/
			if (a_ptr->to_h > ART_W_3) p += 20000;/*Alex*/

			/* Remember, weight is in 0.1 lb. units. */
			if (a_ptr->weight != k_ptr->weight)
				p += (k_ptr->weight - a_ptr->weight) / 20;

			break;
		}
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
			int delta_ac = a_ptr->ac - k_ptr->ac;
			int delta_w = k_ptr->weight - a_ptr->weight;
			p += (a_ptr->ac + 4 * sign(a_ptr->ac)) / 5;
                        if (delta_ac>ART_MAX_EXTRA_AC)
				p+= 20000;
			else
				p += delta_ac;
			if (delta_w)
				p += (delta_w) / 30;
			if (a_ptr->to_a > ART_A_1) p += (a_ptr->to_a - ART_A_1 + 1) / 2;
			if (a_ptr->to_a > ART_A_2) p += (a_ptr->to_a - ART_A_2 + 1) / 2;
			if (a_ptr->to_a > ART_A_3) p += 20000;	/* inhibit */
			break;
		}
		/*Alex: it was p + for nothing
		case TV_LITE:
		{
			p += 10;
			break;
		}
		case TV_RING:
		case TV_AMULET:
		{
			p += 20;
			break;
		}*/
	}

        /*Alex*/
        if (!is_weapon(a_ptr->tval))
        {
                if (a_ptr->flags1 & TR1_BLOWS)
                {
		        if (a_ptr->pval > 3)		
			        p += 20000;	/* inhibit */		
		        else if (a_ptr->pval > 0)		
			        p +=  10 + 10 * a_ptr->pval * a_ptr->pval;/*20 - 50 - 100*/
                }
		p += (a_ptr->to_h + sign(a_ptr->to_h)) / 2;
                if (a_ptr->to_h > ART_1) p += (a_ptr->to_h - ART_1);
                if (a_ptr->to_h > ART_2) p += (a_ptr->to_h - ART_2);
                if (a_ptr->to_h > ART_3) p += 20000;
		p += (a_ptr->to_d + sign(a_ptr->to_d)) / 2;
                if (a_ptr->to_d > ART_1) p += (a_ptr->to_d - ART_1);
                if (a_ptr->to_d > ART_2) p += (a_ptr->to_d - ART_2);
                if (a_ptr->to_d > ART_3) p += 20000;
	}

        if (!is_armour(a_ptr->tval))
        {
		if (a_ptr->to_a > ART_1) p += (a_ptr->to_a - ART_1);
		if (a_ptr->to_a > ART_2) p += (a_ptr->to_a - ART_2);
		if (a_ptr->to_a > ART_3) p += 20000;	/* inhibit */
	}
	/* Other abilities are evaluated independent of the object type. */
	p += (a_ptr->to_a + 3 * sign(a_ptr->to_a)) / 4;

	if (a_ptr->pval > 0)
	{
		if (a_ptr->flags1 & TR1_STR) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_INT) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_WIS) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_DEX) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_CON) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_STEALTH) p += a_ptr->pval * a_ptr->pval;
	}
	else if (a_ptr->pval < 0)	/* hack: don't give large negatives */
	{
		if (a_ptr->flags1 & TR1_STR) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_INT) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_WIS) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_DEX) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_CON) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_STEALTH) p += a_ptr->pval;
	}
	if (a_ptr->flags1 & TR1_CHR) p += a_ptr->pval;
	if (a_ptr->flags1 & TR1_INFRA) p += (a_ptr->pval + sign(a_ptr->pval)) / 2;
	if (a_ptr->flags1 & TR1_SPEED) p += (a_ptr->pval * 3) / 2;

	if (a_ptr->flags2 & TR2_SUST_STR) p += 6;
	if (a_ptr->flags2 & TR2_SUST_INT) p += 4;
	if (a_ptr->flags2 & TR2_SUST_WIS) p += 4;
	if (a_ptr->flags2 & TR2_SUST_DEX) p += 4;
	if (a_ptr->flags2 & TR2_SUST_CON) p += 4;
	if (a_ptr->flags2 & TR2_SUST_CHR) p += 1;
	if (a_ptr->flags2 & TR2_IM_ACID)
	{
		p += 20;
		immunities++;
	}
	if (a_ptr->flags2 & TR2_IM_ELEC)
	{
		p += 24;
		immunities++;
	}
	if (a_ptr->flags2 & TR2_IM_FIRE)
	{
		p += 36;
		immunities++;
	}
	if (a_ptr->flags2 & TR2_IM_COLD)
	{
		p += 24;
		immunities++;
	}
	if (immunities > 1) p += 16;
	if (immunities > 2) p += 16;
	if (immunities > 3) p += 20000;		/* inhibit */
	if (a_ptr->flags3 & TR3_FREE_ACT) p += 8;
	if (a_ptr->flags3 & TR3_HOLD_LIFE) p += 10;
	if (a_ptr->flags2 & TR2_RES_FEAR) p += 1;/*Alex*/
	if (a_ptr->flags2 & TR2_RES_ACID) p += 6;
	if (a_ptr->flags2 & TR2_RES_ELEC) p += 6;
	if (a_ptr->flags2 & TR2_RES_FIRE) p += 6;
	if (a_ptr->flags2 & TR2_RES_COLD) p += 6;
	if (a_ptr->flags2 & TR2_RES_POIS) p += 12;
	if (a_ptr->flags2 & TR2_RES_LITE) p += 8;
	if (a_ptr->flags2 & TR2_RES_DARK) p += 10;
	if (a_ptr->flags2 & TR2_RES_BLIND) p += 10;
	if (a_ptr->flags2 & TR2_RES_CONFU) p += 8;
	if (a_ptr->flags2 & TR2_RES_SOUND) p += 10;
	if (a_ptr->flags2 & TR2_RES_SHARD) p += 8;
	if (a_ptr->flags2 & TR2_RES_NETHR) p += 12;
	if (a_ptr->flags2 & TR2_RES_NEXUS) p += 10;
	if (a_ptr->flags2 & TR2_RES_CHAOS) p += 12;
	if (a_ptr->flags2 & TR2_RES_DISEN) p += 12;

	if (a_ptr->flags3 & TR3_FEATHER) p += 2;
	if (a_ptr->flags3 & TR3_LITE) p += 2;
	if (a_ptr->flags3 & TR3_SEE_INVIS) p += 8;
	if (a_ptr->flags3 & TR3_TELEPATHY) p += 20;
	if (a_ptr->flags3 & TR3_SLOW_DIGEST) p += 4;
	if (a_ptr->flags3 & TR3_REGEN) p += 8;
	if (a_ptr->flags3 & TR3_TELEPORT) p -= 20;
	if (a_ptr->flags3 & TR3_DRAIN_EXP) p -= 16;
	if (a_ptr->flags3 & TR3_AGGRAVATE) p -= 8;
	if (a_ptr->flags3 & TR3_BLESSED) p += 4;
	if (a_ptr->flags3 & TR3_LIGHT_CURSE) p -= 4;
	if (a_ptr->flags3 & TR3_HEAVY_CURSE) p -= 20;
/*	if (a_ptr->flags3 & TR3_PERMA_CURSE) p -= 40; */

	return (p);
}


/*
 * Randomly select a base item type (tval,sval).  Assign the various fields
 * corresponding to that choice.
 * Alex: if (!base){tval and sval from base} 
 */
static void choose_item(int a_idx, const object_type* base)
{
	artifact_type *a_ptr = &a_info[a_idx];
	int tval, sval;
	object_kind *k_ptr;
	int r;
	s16b k_idx, r2;
	byte target_level;

	/*
	 * Look up the original artifact's base object kind to get level and
	 * rarity information to supplement the artifact level/rarity.  As a
	 * degenerate case consider Bladeturner, which has artifact lvl/rar
	 * of only 95/3, but which is based on an object with 110/64!
	 */
	k_idx = kinds[a_idx];
	k_ptr = &k_info[k_idx];
	target_level = k_ptr->level;

	/*
	 * Add base object kind's rarity to artifact rarity.  Later we will
	 * subtract the new object kind's rarity.
	 */
	a_ptr->rarity += k_ptr->chance[0];

	/*
	 * Pick a category (tval) of weapon randomly.  Within each tval, roll
	 * an sval (specific item) based on the target level.  The number we
	 * roll should be a bell curve.  The mean and standard variation of the
	 * bell curve are based on the target level; the distribution of
	 * kinds versus the bell curve is hand-tweaked. :-(
	 */
	r = rand_int(100);

	if (r < 5)
	{
		/* Create a missile weapon. */
		tval = TV_BOW;
		r2 = Rand_normal(target_level * 2, target_level);
		if (r2 < 3) sval = SV_SLING;
		else if (r2 < 10) sval = SV_SHORT_BOW;
		else if (r2 < 30) sval = SV_LONG_BOW;
		else if (r2 < 45) sval = SV_LIGHT_XBOW;
		else sval = SV_HEAVY_XBOW;
	}
	else if (r < 9)
	{
		/* Create a digging tool. */
		tval = TV_DIGGING;
		r2 = Rand_normal(target_level * 2, target_level);
		if (r2 < 15) sval = SV_SHOVEL;
		else if (r2 < 30) sval = SV_PICK;
		else if (r2 < 60) sval = SV_GNOMISH_SHOVEL;
		else if (r2 < 90) sval = SV_ORCISH_PICK;
		else if (r2 < 120) sval = SV_DWARVEN_SHOVEL;
		else sval = SV_DWARVEN_PICK;
	}
	else if (r < 19)
	{
		/* Create a "blunt" weapon. */
		tval = TV_HAFTED;
		r2 = Rand_normal(target_level * 2, target_level);
		if (r2 < 6) sval = SV_WHIP;
		else if (r2 < 12) sval = SV_MACE;
		else if (r2 < 20) sval = SV_WAR_HAMMER;
		else if (r2 < 30) sval = SV_QUARTERSTAFF;
		else if (r2 < 34) sval = SV_LUCERN_HAMMER;
		else if (r2 < 38) sval = SV_MORNING_STAR;
		else if (r2 < 45) sval = SV_FLAIL;
		else if (r2 < 55) sval = SV_LEAD_FILLED_MACE;
		else if (r2 < 80) sval = SV_BALL_AND_CHAIN;
		else if (r2 < 120) sval = SV_TWO_HANDED_FLAIL;
		else sval = SV_MACE_OF_DISRUPTION;
	}
	else if (r < 33)
	{
		/* Create a long, sharp-edged weapon. */
		tval = TV_SWORD;
		r2 = Rand_normal(target_level * 2, target_level);
		if (r2 < 0) sval = SV_BROKEN_DAGGER;
		else if (r2 < 1) sval = SV_BROKEN_SWORD;
		else if (r2 < 5) sval = SV_DAGGER;
		else if (r2 < 9) sval = SV_MAIN_GAUCHE;
		else if (r2 < 10) sval = SV_RAPIER;	/* or at least pointy ;-) */
		else if (r2 < 12) sval = SV_SMALL_SWORD;
		else if (r2 < 14) sval = SV_SHORT_SWORD;
		else if (r2 < 16) sval = SV_SABRE;
		else if (r2 < 18) sval = SV_CUTLASS;
		else if (r2 < 20) sval = SV_TULWAR;
		else if (r2 < 23) sval = SV_BROAD_SWORD;
		else if (r2 < 26) sval = SV_LONG_SWORD;
		else if (r2 < 30) sval = SV_SCIMITAR;
		else if (r2 < 45) sval = SV_BASTARD_SWORD;
		else if (r2 < 60) sval = SV_KATANA;
		else if (r2 < 90) sval = SV_TWO_HANDED_SWORD;
		else if (r2 < 120) sval = SV_EXECUTIONERS_SWORD;
		else sval = SV_BLADE_OF_CHAOS;
	}
	else if (r < 42)
	{
		/* Create a weapon that's not blunt or sword-shaped. */
		tval = TV_POLEARM;
		r2 = Rand_normal(target_level * 2, target_level);
		if (r2 < 12) sval = SV_SPEAR;
		else if (r2 < 20) sval = SV_TRIDENT;
		else if (r2 < 27) sval = SV_LANCE;
		else if (r2 < 35) sval = SV_AWL_PIKE;
		else if (r2 < 45) sval = SV_PIKE;
		else if (r2 < 50) sval = SV_BEAKED_AXE;
		else if (r2 < 55) sval = SV_BROAD_AXE;
		else if (r2 < 60) sval = SV_BATTLE_AXE;
		else if (r2 < 65) sval = SV_GLAIVE;
		else if (r2 < 80) sval = SV_HALBERD;
		else if (r2 < 120) sval = SV_GREAT_AXE;
		else if (r2 < 128) sval = SV_SCYTHE;
		else if (r2 < 135) sval = SV_LOCHABER_AXE;
		else sval = SV_SCYTHE_OF_SLICING;
	}
	else if (r < 64)
	{
		/* Create light or hard body armor. */
		r2 = Rand_normal(target_level * 2, target_level);
		if (r2 < 45) tval = TV_SOFT_ARMOR; else tval = TV_HARD_ARMOR;

		/* Soft stuff. */
		if (r2 < 0) sval = SV_FILTHY_RAG;
		else if (r2 < 5) sval = SV_ROBE;
		else if (r2 < 10) sval = SV_SOFT_LEATHER_ARMOR;
		else if (r2 < 15) sval = SV_SOFT_STUDDED_LEATHER;
		else if (r2 < 20) sval = SV_HARD_LEATHER_ARMOR;
		else if (r2 < 30) sval = SV_HARD_STUDDED_LEATHER;
		else if (r2 < 45) sval = SV_LEATHER_SCALE_MAIL;

		/* Hard stuff. */
		else if (r2 < 55) sval = SV_RUSTY_CHAIN_MAIL;
		else if (r2 < 65) sval = SV_METAL_SCALE_MAIL;
		else if (r2 < 75) sval = SV_CHAIN_MAIL;
		else if (r2 < 85) sval = SV_AUGMENTED_CHAIN_MAIL;
		else if (r2 < 90) sval = SV_DOUBLE_CHAIN_MAIL;
		else if (r2 < 97) sval = SV_BAR_CHAIN_MAIL;
		else if (r2 < 105) sval = SV_METAL_BRIGANDINE_ARMOUR;
		else if (r2 < 115) sval = SV_PARTIAL_PLATE_ARMOUR;
		else if (r2 < 125) sval = SV_METAL_LAMELLAR_ARMOUR;
		else if (r2 < 135) sval = SV_FULL_PLATE_ARMOUR;
		else if (r2 < 140) sval = SV_RIBBED_PLATE_ARMOUR;
		else if (r2 < 150) sval = SV_MITHRIL_CHAIN_MAIL;
		else if (r2 < 170) sval = SV_MITHRIL_PLATE_MAIL;
		else sval = SV_ADAMANTITE_PLATE_MAIL;
	}
	else if (r < 71)
	{
		/* Make shoes. */
		tval = TV_BOOTS;
		r2 = Rand_normal(target_level * 2, target_level);
		if (r2 < 9) sval = SV_PAIR_OF_SOFT_LEATHER_BOOTS;
		else if (r2 < 15) sval = SV_PAIR_OF_HARD_LEATHER_BOOTS;
		else sval = SV_PAIR_OF_METAL_SHOD_BOOTS;
	}
	else if (r < 78)
	{
		/* Make gloves. */
		tval = TV_GLOVES;
		r2 = Rand_normal(target_level * 2, target_level);
		if (r2 < 10) sval = SV_SET_OF_LEATHER_GLOVES;
		else if (r2 < 30) sval = SV_SET_OF_GAUNTLETS;
		else sval = SV_SET_OF_CESTI;
	}
	else if (r < 87)
	{
		/* Make headgear. */
		r2 = Rand_normal(target_level * 2, target_level);
		if (r2 < 50) tval = TV_HELM; else tval = TV_CROWN;

		if (r2 < 9) sval = SV_HARD_LEATHER_CAP;
		else if (r2 < 20) sval = SV_METAL_CAP;
		else if (r2 < 40) sval = SV_IRON_HELM;
		else if (r2 < 50) sval = SV_STEEL_HELM;

		else if (r2 < 60) sval = SV_IRON_CROWN;
		else if (r2 < 90) sval = SV_GOLDEN_CROWN;
		else sval = SV_JEWELED_CROWN;
	}
	else if (r < 94)
	{
		/* Make a shield. */
		tval = TV_SHIELD;
		r2 = Rand_normal(target_level * 2, target_level);
		if (r2 < 9) sval = SV_SMALL_LEATHER_SHIELD;
		else if (r2 < 20) sval = SV_SMALL_METAL_SHIELD;
		else if (r2 < 40) sval = SV_LARGE_LEATHER_SHIELD;
		else if (r2 < 60) sval = SV_LARGE_METAL_SHIELD;
		else sval = SV_SHIELD_OF_DEFLECTION;
	}
	else
	{
		/* Make a cloak. */
		tval = TV_CLOAK;
		r2 = Rand_normal(target_level * 2, target_level);
		if (r2 < 90) sval = SV_CLOAK;
		else sval = SV_SHADOW_CLOAK;
	}

        if (base){
                tval = base->tval;
                sval = base->sval;
        }

	k_idx = lookup_kind(tval, sval);
	k_ptr = &k_info[k_idx];
	kinds[a_idx] = k_idx;

	/*
	 * Subtract the new object kind's rarity (see above).  We can't
	 * blindly subtract, because a_ptr->rarity is a byte.
	 */
	if (a_ptr->rarity <= k_ptr->chance[0])
		a_ptr->rarity = 1;
	else
		a_ptr->rarity -= k_ptr->chance[0];

	a_ptr->tval = k_ptr->tval;
	a_ptr->sval = k_ptr->sval;
	a_ptr->pval = k_ptr->pval;
	a_ptr->to_h = k_ptr->to_h;
	a_ptr->to_d = k_ptr->to_d;
	a_ptr->to_a = k_ptr->to_a;
	a_ptr->ac = k_ptr->ac;
	a_ptr->dd = k_ptr->dd;
	a_ptr->ds = k_ptr->ds;
	a_ptr->weight = k_ptr->weight;
	a_ptr->flags1 = k_ptr->flags1;
	a_ptr->flags2 = k_ptr->flags2;
	a_ptr->flags3 = k_ptr->flags3;

	/* Artifacts ignore everything */
	a_ptr->flags3 |= TR3_IGNORE_MASK;

	/* Assign basic stats to the artifact based on its artifact level. */
	switch (a_ptr->tval)
	{
		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_SWORD:
		case TV_POLEARM:
			a_ptr->to_h += (s16b)(a_ptr->level / 10 + rand_int(4) +
			                      rand_int(4));
			a_ptr->to_d += (s16b)(a_ptr->level / 10 + rand_int(4));
			a_ptr->to_d += (s16b)(rand_int((a_ptr->dd * a_ptr->ds) / 2 + 1));
			break;
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
			a_ptr->to_a += (s16b)(a_ptr->level / 10 + a_ptr->ac / 3 +
			                      rand_int(8));

			if (a_ptr->to_a < 10)
				a_ptr->to_a += (s16b)(2 + rand_int(4) + rand_int(4));

				/*
			 * Make sure armor gets some resists!  Hard body armor
			 * is generally high-level stuff, with good ac and
			 * to_a.  That sucks up all the points....
			 */
			switch (a_ptr->tval)
			{
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
				if (rand_int(2) == 0) a_ptr->flags2 |= TR2_RES_ACID;
				if (rand_int(2) == 0) a_ptr->flags2 |= TR2_RES_ELEC;
				if (rand_int(2) == 0) a_ptr->flags2 |= TR2_RES_COLD;
				if (rand_int(2) == 0) a_ptr->flags2 |= TR2_RES_FIRE;
				break;
			}
			break;
	}
}

/* Alex: do_pval can increase pval<0 when cursed_art = FALSE */
bool cursed_art = FALSE;
/* Alex: string with new artifact property */
enum {LPROP = 80};
char new_property[LPROP];

/* Alex: string concatenation with range checking */
static void strcat_check(char* s, const char* t, int max_length){
        char* max_chr = &(s[max_length-1]);
        while (*s && s!= max_chr) s++;/* Now s points to '\0' or to last buffer symbol */
        while (*t && s != max_chr) *s++ = *t++;
        *s = '\0';
}

/*
 * We've just added an ability which uses the pval bonus.  Make sure it's
 * not zero.  If it's currently negative, leave it negative (heh heh).
 */
static void do_pval(artifact_type *a_ptr)
{
	if (!a_ptr->pval) a_ptr->pval = (s16b)(1 + rand_int(3));
	else if (a_ptr->pval < 0 && cursed_art)
	{
		if (rand_int(2) == 0) a_ptr->pval--;
	}
	else if (rand_int(3) == 0)
        {
                if (a_ptr->pval != -1)
                        a_ptr->pval++;
                else
                        /* Alex: make sure it's not zero. */
                        a_ptr->pval = 1;
        }
}


static void remove_contradictory(artifact_type *a_ptr)
{
	if (a_ptr->flags3 & TR3_AGGRAVATE) a_ptr->flags1 &= ~(TR1_STEALTH);
	if (a_ptr->flags2 & TR2_IM_ACID) a_ptr->flags2 &= ~(TR2_RES_ACID);
	if (a_ptr->flags2 & TR2_IM_ELEC) a_ptr->flags2 &= ~(TR2_RES_ELEC);
	if (a_ptr->flags2 & TR2_IM_FIRE) a_ptr->flags2 &= ~(TR2_RES_FIRE);
	if (a_ptr->flags2 & TR2_IM_COLD) a_ptr->flags2 &= ~(TR2_RES_COLD);

	if (a_ptr->pval < 0)
	{
		if (a_ptr->flags1 & TR1_STR) a_ptr->flags2 &= ~(TR2_SUST_STR);
		if (a_ptr->flags1 & TR1_INT) a_ptr->flags2 &= ~(TR2_SUST_INT);
		if (a_ptr->flags1 & TR1_WIS) a_ptr->flags2 &= ~(TR2_SUST_WIS);
		if (a_ptr->flags1 & TR1_DEX) a_ptr->flags2 &= ~(TR2_SUST_DEX);
		if (a_ptr->flags1 & TR1_CON) a_ptr->flags2 &= ~(TR2_SUST_CON);
		if (a_ptr->flags1 & TR1_CHR) a_ptr->flags2 &= ~(TR2_SUST_CHR);
		a_ptr->flags1 &= ~(TR1_BLOWS);
	}

	if (a_ptr->flags3 & TR3_LIGHT_CURSE) a_ptr->flags3 &= ~(TR3_BLESSED);
	if (a_ptr->flags1 & TR1_KILL_DRAGON) a_ptr->flags1 &= ~(TR1_SLAY_DRAGON);
	if (a_ptr->flags1 & TR1_KILL_DEMON) a_ptr->flags1 &= ~(TR1_SLAY_DEMON);
	if (a_ptr->flags1 & TR1_KILL_UNDEAD) a_ptr->flags1 &= ~(TR1_SLAY_UNDEAD);
	if (a_ptr->flags3 & TR3_DRAIN_EXP) a_ptr->flags3 &= ~(TR3_HOLD_LIFE);
}

static void add_search(artifact_type *a_ptr)
{
        if (!a_ptr->pval) a_ptr->pval = (s16b)(1 + rand_int(3));
        /*Alex: do_pval() often leads to pval == 2000 or so
        when artifact has only searching pval
	do_pval(a_ptr);*/
        if (a_ptr->flags1 & TR1_SEARCH) return;
        a_ptr->flags1 |= TR1_SEARCH;
        strcat_check(new_property, " Searching", LPROP);
}
static void add_res_fear(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_FEAR) return;
        a_ptr->flags2 |= TR2_RES_FEAR;
        strcat_check(new_property, " Resist Fear", LPROP);
}
static void add_infra(artifact_type *a_ptr)
{
        do_pval(a_ptr);
        if (a_ptr->flags1 & TR1_INFRA) return;
        a_ptr->flags1 |= TR1_INFRA;
        strcat_check(new_property, " Infravision", LPROP);
}
static void add_sust_str(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_SUST_STR) return;
        a_ptr->flags2 |= TR2_SUST_STR;
        strcat_check(new_property, " Sustain Strength", LPROP);
}
static void add_sust_int(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_SUST_INT) return;
        a_ptr->flags2 |= TR2_SUST_INT;
        strcat_check(new_property, " Sustain Intelligence", LPROP);
}
static void add_sust_wis(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_SUST_WIS) return;
        a_ptr->flags2 |= TR2_SUST_WIS;
        strcat_check(new_property, " Sustain Wisdom", LPROP);
}
static void add_sust_dex(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_SUST_DEX) return;
        a_ptr->flags2 |= TR2_SUST_DEX;
        strcat_check(new_property, " Sustain Dexterity", LPROP);
}
static void add_sust_con(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_SUST_CON) return;
        a_ptr->flags2 |= TR2_SUST_CON;
        strcat_check(new_property, " Sustain Constitution", LPROP);
}
static void add_sust_chr(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_SUST_CHR) return;
        a_ptr->flags2 |= TR2_SUST_CHR;
        strcat_check(new_property, " Sustain Charisma", LPROP);
}
static void add_res_acid(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_ACID) return;
        a_ptr->flags2 |= TR2_RES_ACID;
        strcat_check(new_property, " Resist Acid", LPROP);
}
static void add_res_elec(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_ELEC) return;
        a_ptr->flags2 |= TR2_RES_ELEC;
        strcat_check(new_property, " Resist Electricity", LPROP);
}
static void add_res_fire(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_FIRE) return;
        a_ptr->flags2 |= TR2_RES_FIRE;
        strcat_check(new_property, " Resist Fire", LPROP);
}
static void add_res_cold(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_COLD) return;
        a_ptr->flags2 |= TR2_RES_COLD;
        strcat_check(new_property, " Resist Cold", LPROP);
}
static void add_feather(artifact_type *a_ptr)
{
        if (a_ptr->flags3 & TR3_FEATHER) return;
        a_ptr->flags3 |= TR3_FEATHER;
        strcat_check(new_property, " Feather falling", LPROP);
}
static void add_lite(artifact_type *a_ptr)
{
        if (a_ptr->flags3 & TR3_LITE) return;
        a_ptr->flags3 |= TR3_LITE;
        strcat_check(new_property, " Lite", LPROP);
}
static void add_see_invis(artifact_type *a_ptr)
{
        if (a_ptr->flags3 & TR3_SEE_INVIS) return;
        a_ptr->flags3 |= TR3_SEE_INVIS;
        strcat_check(new_property, " See invisible", LPROP);
}
static void add_slow_digest(artifact_type *a_ptr)
{
        if (a_ptr->flags3 & TR3_SLOW_DIGEST) return;
        a_ptr->flags3 |= TR3_SLOW_DIGEST;
        strcat_check(new_property, " Slow digestion", LPROP);
}

static void remove_drain_exp(artifact_type *a_ptr)
{
        if (!(a_ptr->flags3 & TR3_DRAIN_EXP)) return;
        a_ptr->flags3 &= ~(TR3_DRAIN_EXP);
        strcat_check(new_property, " Removed experience drain", LPROP);
}
static void remove_teleport(artifact_type *a_ptr)
{
        if (!(a_ptr->flags3 & TR3_TELEPORT)) return;
        a_ptr->flags3 &= ~(TR3_TELEPORT);
        strcat_check(new_property, " Removed random teleportation", LPROP);
}
static void add_drain_exp(artifact_type *a_ptr)
{
        if (a_ptr->flags3 & TR3_DRAIN_EXP) return;
        a_ptr->flags3 |= TR3_DRAIN_EXP;
        strcat_check(new_property, " Experience drain", LPROP);
}
static void add_teleport(artifact_type *a_ptr)
{
        if (a_ptr->flags3 & TR3_TELEPORT) return;
        a_ptr->flags3 |= TR3_TELEPORT;
        strcat_check(new_property, " Random teleportation", LPROP);
}

static void add_common_ability(artifact_type *a_ptr)
{
		int r = rand_int(21);
		switch (r)
		{
			case 0:add_search(a_ptr);break;
			case 1: add_infra(a_ptr);break;

			case 2: add_sust_str(a_ptr); break;
			case 3: add_sust_int(a_ptr); break;
			case 4: add_sust_wis(a_ptr); break;
			case 5: add_sust_dex(a_ptr); break;
			case 6: add_sust_con(a_ptr); break;
			case 7: add_sust_chr(a_ptr); break;

			case 8: add_res_acid(a_ptr); break;
			case 9: add_res_elec(a_ptr); break;
			case 10: add_res_fire(a_ptr); break;
			case 11: add_res_cold(a_ptr); break;

			case 12: add_feather(a_ptr); break;
			case 13: add_lite(a_ptr); break;
			case 14: add_see_invis(a_ptr); break;
			case 15: add_slow_digest(a_ptr); break;

			case 16: if (!rand_int(5)) add_drain_exp(a_ptr);break;
			case 17: if (!rand_int(10)) remove_drain_exp(a_ptr);break;
			case 18: if (!rand_int(5)) add_teleport(a_ptr);break;
			case 19: if (!rand_int(10)) remove_teleport(a_ptr);break;

			case 20: add_res_fear(a_ptr); break;
		}/*switch (r)*/
}

static void add_blessed(artifact_type *a_ptr)
{
        if (a_ptr->flags3 & TR3_BLESSED) return;
        a_ptr->flags3 |= TR3_BLESSED;
        strcat_check(new_property, " Blessed", LPROP);
}
static void add_str(artifact_type *a_ptr)
{
        do_pval(a_ptr);
        if (rand_int(3) == 0) add_sust_str(a_ptr);
        if (a_ptr->flags1 & TR1_STR) return;
        a_ptr->flags1 |= TR1_STR;
        strcat_check(new_property, " Strength", LPROP);
}
static void add_int(artifact_type *a_ptr)
{
        do_pval(a_ptr);
        if (rand_int(3) == 0) add_sust_int(a_ptr);
        if (a_ptr->flags1 & TR1_INT) return;
        a_ptr->flags1 |= TR1_INT;
        strcat_check(new_property, " Intelligence", LPROP);
}
static void add_wis(artifact_type *a_ptr)
{
        do_pval(a_ptr);
        if (rand_int(3) == 0) add_sust_wis(a_ptr);
        if ((a_ptr->tval == TV_SWORD) || (a_ptr->tval == TV_POLEARM))
                add_blessed(a_ptr);
        if (a_ptr->flags1 & TR1_WIS) return;
        a_ptr->flags1 |= TR1_WIS;
        strcat_check(new_property, " Wisdom", LPROP);
}
static void add_dex(artifact_type *a_ptr)
{
        do_pval(a_ptr);
        if (rand_int(3) == 0) add_sust_dex(a_ptr);
        if (a_ptr->flags1 & TR1_DEX) return;
        a_ptr->flags1 |= TR1_DEX;
        strcat_check(new_property, " Dexterity", LPROP);
}
static void add_con(artifact_type *a_ptr)
{
        do_pval(a_ptr);
        if (rand_int(3) == 0) add_sust_con(a_ptr);
        if (a_ptr->flags1 & TR1_CON) return;
        a_ptr->flags1 |= TR1_CON;
        strcat_check(new_property, " Constitution", LPROP);
}
static void add_chr(artifact_type *a_ptr)
{
        do_pval(a_ptr);
        if (rand_int(3) == 0) add_sust_chr(a_ptr);
        if (a_ptr->flags1 & TR1_CHR) return;
        a_ptr->flags1 |= TR1_CHR;
        strcat_check(new_property, " Charisma", LPROP);
}
static void add_stealth(artifact_type *a_ptr)
{
	do_pval(a_ptr);
        if (a_ptr->flags1 & TR1_STEALTH) return;
        a_ptr->flags1 |= TR1_STEALTH;
        strcat_check(new_property, " Stealth", LPROP);
}
static void add_res_pois(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_POIS) return;
        a_ptr->flags2 |= TR2_RES_POIS;
        strcat_check(new_property, " Resist Poison", LPROP);
}
static void add_res_lite(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_LITE) return;
        a_ptr->flags2 |= TR2_RES_LITE;
        strcat_check(new_property, " Resist Lite", LPROP);
}
static void add_res_dark(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_DARK) return;
        a_ptr->flags2 |= TR2_RES_DARK;
        strcat_check(new_property, " Resist Darkness", LPROP);
}
static void add_res_blind(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_BLIND) return;
        a_ptr->flags2 |= TR2_RES_BLIND;
        strcat_check(new_property, " Resist Blindness", LPROP);
}
static void add_regen(artifact_type *a_ptr)
{
        if (a_ptr->flags3 & TR3_REGEN) return;
        a_ptr->flags3 |= TR3_REGEN;
        strcat_check(new_property, " Regeneration", LPROP);
}
static void add_sust_all(artifact_type *a_ptr)
{
        if ((a_ptr->flags2 & TR2_SUST_STR) &&
             (a_ptr->flags2 & TR2_SUST_INT) &&
             (a_ptr->flags2 & TR2_SUST_WIS) &&
             (a_ptr->flags2 & TR2_SUST_DEX) &&
             (a_ptr->flags2 & TR2_SUST_CON) &&
             (a_ptr->flags2 & TR2_SUST_CHR))
                return;
        a_ptr->flags2 |= TR2_SUST_STR;
        a_ptr->flags2 |= TR2_SUST_INT;
        a_ptr->flags2 |= TR2_SUST_WIS;
        a_ptr->flags2 |= TR2_SUST_DEX;
        a_ptr->flags2 |= TR2_SUST_CON;
        a_ptr->flags2 |= TR2_SUST_CHR;
        strcat_check(new_property, " Sustain all", LPROP);
}
static void add_uncommon_ability(artifact_type *a_ptr)
{
		int r = rand_int(13);
		switch (r)
		{
			case 0: add_str(a_ptr); break;
			case 1: add_int(a_ptr); break;
			case 2: add_wis(a_ptr); break;
			case 3: add_dex(a_ptr); break;
			case 4: add_con(a_ptr); break;
			case 5: add_chr(a_ptr); break;

			case 6: add_stealth(a_ptr); break;
			case 7: add_res_pois(a_ptr); break;
			case 8: add_res_lite(a_ptr); break;
			case 9: add_res_dark(a_ptr); break;
			case 10: add_res_blind(a_ptr); break;
			case 11: add_regen(a_ptr); break;
                        case 12: add_sust_all(a_ptr); break;
		}/*switch (r)*/
}

static void add_speed(artifact_type *a_ptr, int val)
{
        if (a_ptr->pval == 0) a_ptr->pval = val;
	else do_pval(a_ptr);
        if (a_ptr->flags1 & TR1_SPEED) return;
        a_ptr->flags1 |= TR1_SPEED;
        strcat_check(new_property, " Speed", LPROP);
}
static void add_free_act(artifact_type *a_ptr)
{
        if (a_ptr->flags3 & TR3_FREE_ACT) return;
        a_ptr->flags3 |= TR3_FREE_ACT;
        strcat_check(new_property, " Free Action", LPROP);
}
static void add_hold_life(artifact_type *a_ptr)
{
        if (a_ptr->flags3 & TR3_HOLD_LIFE) return;
        a_ptr->flags3 |= TR3_HOLD_LIFE;
        strcat_check(new_property, " Hold Life", LPROP);
}
static void add_res_confu(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_CONFU) return;
        a_ptr->flags2 |= TR2_RES_CONFU;
        strcat_check(new_property, " Resist Confusion", LPROP);
}
static void add_res_sound(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_SOUND) return;
        a_ptr->flags2 |= TR2_RES_SOUND;
        strcat_check(new_property, " Resist Sound", LPROP);
}
static void add_res_shard(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_SHARD) return;
        a_ptr->flags2 |= TR2_RES_SHARD;
        strcat_check(new_property, " Resist Shards", LPROP);
}
static void add_res_nexus(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_NEXUS) return;
        a_ptr->flags2 |= TR2_RES_NEXUS;
        strcat_check(new_property, " Resist Nexus", LPROP);
}
static void add_res_chaos(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_CHAOS) return;
        a_ptr->flags2 |= TR2_RES_CHAOS;
        strcat_check(new_property, " Resist Chaos", LPROP);
}
static void add_resistance(artifact_type *a_ptr)
{
        if ((a_ptr->flags2 & TR2_RES_ACID) &&
             (a_ptr->flags2 & TR2_RES_ELEC) &&
             (a_ptr->flags2 & TR2_RES_FIRE) &&
             (a_ptr->flags2 & TR2_RES_COLD) &&
             (a_ptr->flags2 & TR2_RES_POIS))
              return;
        a_ptr->flags2 |= TR2_RES_ACID;
        a_ptr->flags2 |= TR2_RES_ELEC;
        a_ptr->flags2 |= TR2_RES_FIRE;
        a_ptr->flags2 |= TR2_RES_COLD;
        a_ptr->flags2 |= TR2_RES_POIS;
        strcat_check(new_property, " Resistance", LPROP);
}

static void add_rare_ability(artifact_type *a_ptr)
{
		int r = rand_int(9);
		switch (r)
		{
			case 0: add_speed(a_ptr, 3+ rand_int(3)); break;
			case 1: add_free_act(a_ptr); break;
			case 2: add_hold_life(a_ptr); break;
			case 3: add_res_confu(a_ptr); break;
			case 4: add_res_sound(a_ptr); break;
			case 5: add_res_shard(a_ptr); break;
			case 6: add_res_nexus(a_ptr); break;
			case 7: add_res_chaos(a_ptr); break;
                        case 8: add_resistance(a_ptr); break;
		}/*switch (r)*/
}

static void add_im_acid(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_IM_ACID) return;
        a_ptr->flags2 |= TR2_IM_ACID;
        strcat_check(new_property, " Immunity to Acid", LPROP);
}
static void add_im_elec(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_IM_ELEC) return;
        a_ptr->flags2 |= TR2_IM_ELEC;
        strcat_check(new_property, " Immunity to Electricity", LPROP);
}
static void add_im_fire(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_IM_FIRE) return;
        a_ptr->flags2 |= TR2_IM_FIRE;
        strcat_check(new_property, " Immunity to Fire", LPROP);
}
static void add_im_cold(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_IM_COLD) return;
        a_ptr->flags2 |= TR2_IM_COLD;
        strcat_check(new_property, " Immunity to Cold", LPROP);
}
static void add_res_nether(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_NETHR) return;
        a_ptr->flags2 |= TR2_RES_NETHR;
        strcat_check(new_property, " Resist Nether", LPROP);
}
static void add_res_disen(artifact_type *a_ptr)
{
        if (a_ptr->flags2 & TR2_RES_DISEN) return;
        a_ptr->flags2 |= TR2_RES_DISEN;
        strcat_check(new_property, " Resist Disenchantment", LPROP);
}
static void add_telepathy(artifact_type *a_ptr)
{
        if (a_ptr->flags3 & TR3_TELEPATHY) return;
        a_ptr->flags3 |= TR3_TELEPATHY;
        strcat_check(new_property, " Telepathy", LPROP);
}
static void add_blow(artifact_type *a_ptr)
{
        do_pval(a_ptr);
        if (a_ptr->flags1 & TR1_BLOWS) return;
        a_ptr->flags1 |= TR1_BLOWS;
        strcat_check(new_property, " Blows", LPROP);
}
static void add_to_h_to_d(artifact_type *a_ptr, int plus)
{
        /* Alex: it is impossible to overflow 80-char array by (+xxx,+xxx) */
        char buf[80];
        char *t = buf;
	a_ptr->to_d += plus;
	a_ptr->to_h += plus;
	a_ptr->flags3 |= TR3_SHOW_MODS;
        object_desc_str_macro(t, " (");
        object_desc_int_macro(t, plus);
        object_desc_chr_macro(t, ',');
        object_desc_int_macro(t, plus);
        object_desc_chr_macro(t, ')');
        object_desc_chr_macro(t, '\0');
        strcat_check(new_property, buf, LPROP);
}
static void add_all_stats(artifact_type *a_ptr)
{
        do_pval(a_ptr);
        if ((a_ptr->flags1 & TR1_STR) &&
	    (a_ptr->flags1 & TR1_INT) &&
	    (a_ptr->flags1 & TR1_WIS) &&
	    (a_ptr->flags1 & TR1_DEX) &&
	    (a_ptr->flags1 & TR1_CON) &&
	    (a_ptr->flags1 & TR1_CHR)) return;
        a_ptr->flags1 |= TR1_STR;
        a_ptr->flags1 |= TR1_INT;
        a_ptr->flags1 |= TR1_WIS;
        a_ptr->flags1 |= TR1_DEX;
        a_ptr->flags1 |= TR1_CON;
        a_ptr->flags1 |= TR1_CHR;
        strcat_check(new_property, " All Stats", LPROP);
}
static void add_very_rare_ability(artifact_type *a_ptr)
{
		int r = rand_int(10);
		switch (r)
		{
			case 0: add_im_acid(a_ptr); break;
			case 1: add_im_elec(a_ptr); break;
			case 2: add_im_fire(a_ptr); break;
			case 3: add_im_cold(a_ptr); break;
			case 4: add_res_nether(a_ptr);break;
			case 5: add_res_disen(a_ptr); break;
			case 6: add_telepathy(a_ptr);break;
			case 7: if (!rand_int(3)) add_blow(a_ptr); break;
                        case 8: if (!rand_int(3)) add_to_h_to_d(a_ptr, (3 + rand_int(3) + rand_int(3) + rand_int(3))); break;
			case 9: if (!rand_int(2)) add_all_stats(a_ptr); break;
		}/*switch (r)*/
}

static void add_to_h(artifact_type *a_ptr, int plus)
{
        /* Alex: it is impossible to overflow 80-char array by (+xxx,+0) */
        char buf[80];
        char *t = buf;
	a_ptr->to_h += plus;
	a_ptr->flags3 |= TR3_SHOW_MODS;
        object_desc_str_macro(t, " (");
        object_desc_int_macro(t, plus);
        object_desc_str_macro(t, ",+0)");
        object_desc_chr_macro(t, '\0');
        strcat_check(new_property, buf, LPROP);
}
static void add_to_d(artifact_type *a_ptr, int plus)
{
        /* Alex: it is impossible to overflow 80-char array by (+0,+xxx) */
        char buf[80];
        char *t = buf;
	a_ptr->to_d += plus;
	a_ptr->flags3 |= TR3_SHOW_MODS;
        object_desc_str_macro(t, " (+0,");
        object_desc_int_macro(t, plus);
        object_desc_chr_macro(t, ')');
        object_desc_chr_macro(t, '\0');
        strcat_check(new_property, buf, LPROP);
}
static void add_to_a(artifact_type *a_ptr, int plus)
{
        /* Alex: it is impossible to overflow 80-char array by [+0,+xxx] */
        char buf[80];
        char *t = buf;
	a_ptr->to_a += plus;
        object_desc_str_macro(t, " [+0,");
        object_desc_int_macro(t, plus);
        object_desc_chr_macro(t, ']');
        object_desc_chr_macro(t, '\0');
        strcat_check(new_property, buf, LPROP);
}
static void add_slay_orc(artifact_type *a_ptr);
static void add_slay_troll(artifact_type *a_ptr);
static void add_slay_giant(artifact_type *a_ptr);

static void add_slay_orc(artifact_type *a_ptr)
{
	if (rand_int(2) == 0) add_slay_troll(a_ptr);
	if (rand_int(2) == 0) add_slay_giant(a_ptr);
        if (a_ptr->flags1 & TR1_SLAY_ORC) return;
        a_ptr->flags1 |= TR1_SLAY_ORC;
        strcat_check(new_property, " Slay Orks", LPROP);
}
static void add_slay_troll(artifact_type *a_ptr)
{
	if (rand_int(2) == 0) add_slay_orc(a_ptr);
	if (rand_int(2) == 0) add_slay_giant(a_ptr);
        if (a_ptr->flags1 & TR1_SLAY_TROLL) return;
        a_ptr->flags1 |= TR1_SLAY_TROLL;
        strcat_check(new_property, " Slay Trolls", LPROP);
}
static void add_slay_giant(artifact_type *a_ptr)
{
	if (rand_int(2) == 0) add_slay_troll(a_ptr);
	if (rand_int(2) == 0) add_slay_orc(a_ptr);
        if (a_ptr->flags1 & TR1_SLAY_GIANT) return;
        a_ptr->flags1 |= TR1_SLAY_GIANT;
        strcat_check(new_property, " Slay Giants", LPROP);
}
static void add_low_weight(artifact_type *a_ptr)
{
        a_ptr->weight = (a_ptr->weight * 9) / 10;
        strcat_check(new_property, " 90% Weight", LPROP);
}
static void add_tunnel(artifact_type *a_ptr)
{
	do_pval(a_ptr);
        if (a_ptr->flags1 & TR1_TUNNEL) return;
        a_ptr->flags1 |= TR1_TUNNEL;
        strcat_check(new_property, " Tunneling", LPROP);
}
static void add_common_specific(artifact_type *a_ptr)
{
		int r = rand_int(100);
		switch (a_ptr->tval)
		{
			case TV_BOW:
			{
				if (r < 50) add_to_h(a_ptr, 2 + rand_int(2));
				else add_to_d(a_ptr, 2 + rand_int(3));
				break;
			}
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			{
				if (r < 6) add_blessed(a_ptr);
				else if (r < 19) add_slay_orc(a_ptr);
				else if (r < 32) add_slay_troll(a_ptr);
				else if (r < 45) add_slay_giant(a_ptr);
				else if (r < 60) add_to_h(a_ptr, 3 + rand_int(4));
				else if (r < 80) add_to_d(a_ptr, 3 + rand_int(4));
				else if (r < 90) add_low_weight(a_ptr);
				else add_tunnel(a_ptr);
				break;
			}
			case TV_BOOTS:
			{
				if (r < 35) add_feather(a_ptr);
				else if (r < 85) add_to_a(a_ptr, 2 + rand_int(4));
				else add_low_weight(a_ptr);
				break;
			}
			case TV_GLOVES:
			{
				add_to_a(a_ptr, 3 + rand_int(3));
				break;
			}
			case TV_HELM:
			case TV_CROWN:
			{
				if (r < 20) add_res_blind(a_ptr);
				else if (r < 50) add_see_invis(a_ptr);
				else add_to_a(a_ptr, 3 + rand_int(3));
				break;
			}
			case TV_SHIELD:
			{
				if (r < 20) add_res_acid(a_ptr);
				else if (r < 40) add_res_elec(a_ptr);
				else if (r < 60) add_res_fire(a_ptr);
				else if (r < 80) add_res_cold(a_ptr);
				else add_to_a(a_ptr, 3 + rand_int(3));
				break;
			}
			case TV_CLOAK:
			{
				add_to_a(a_ptr, 3 + rand_int(3));
				break;
			}
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			{
				if (r < 15) add_res_acid(a_ptr);
				else if (r < 30) add_res_elec(a_ptr);
				else if (r < 45) add_res_fire(a_ptr);
				else if (r < 60) add_res_cold(a_ptr);
				else if (r < 70) add_low_weight(a_ptr);
				else add_to_a(a_ptr, 3 + rand_int(3));
				break;
			}
			case TV_RING:
			case TV_AMULET:
                        {
				if (r < 10) add_res_acid(a_ptr);
				else if (r < 20) add_res_elec(a_ptr);
				else if (r < 30) add_res_fire(a_ptr);
				else if (r < 40) add_res_cold(a_ptr);
                                else if (r < 50) add_sust_str(a_ptr);
                                else if (r < 60) add_sust_int(a_ptr);
                                else if (r < 70) add_sust_wis(a_ptr);
                                else if (r < 80) add_sust_dex(a_ptr);
                                else if (r < 90) add_sust_con(a_ptr);
                                else add_sust_chr(a_ptr);
                                break;
                        }
			case TV_LITE:
                        {
                                if (r<40) add_lite(a_ptr);
                                else if (r<70) add_res_lite(a_ptr);
                                else add_res_dark(a_ptr);
                                break;
                        }
		}/*switch (a_ptr->tval)*/
}
static void add_might(artifact_type *a_ptr)
{
	do_pval(a_ptr);
        if (a_ptr->flags1 & TR1_MIGHT) return;
        a_ptr->flags1 |= TR1_MIGHT;
        strcat_check(new_property, " Might", LPROP);
}
static void add_acid(artifact_type *a_ptr)
{
        if (rand_int(4) > 0) add_res_acid(a_ptr);
        if (a_ptr->flags1 & TR1_BRAND_ACID) return;
        a_ptr->flags1 |= TR1_BRAND_ACID;
        strcat_check(new_property, " Acid", LPROP);
}
static void add_elec(artifact_type *a_ptr)
{
        if (rand_int(4) > 0) add_res_elec(a_ptr);
        if (a_ptr->flags1 & TR1_BRAND_ELEC) return;
        a_ptr->flags1 |= TR1_BRAND_ELEC;
        strcat_check(new_property, " Lightning", LPROP);
}
static void add_fire(artifact_type *a_ptr)
{
        if (rand_int(4) > 0) add_res_fire(a_ptr);
        if (a_ptr->flags1 & TR1_BRAND_FIRE) return;
        a_ptr->flags1 |= TR1_BRAND_FIRE;
        strcat_check(new_property, " Fire", LPROP);
}
static void add_cold(artifact_type *a_ptr)
{
        if (rand_int(4) > 0) add_res_cold(a_ptr);
        if (a_ptr->flags1 & TR1_BRAND_COLD) return;
        a_ptr->flags1 |= TR1_BRAND_COLD;
        strcat_check(new_property, " Frost", LPROP);
}
static void add_slay_dragon(artifact_type *a_ptr)
{
        if (a_ptr->flags1 & TR1_SLAY_DRAGON) return;
        a_ptr->flags1 |= TR1_SLAY_DRAGON;
        strcat_check(new_property, " Slay Dragons", LPROP);
}
static void add_slay_evil(artifact_type *a_ptr)
{
        if (a_ptr->flags1 & TR1_SLAY_EVIL) return;
        a_ptr->flags1 |= TR1_SLAY_EVIL;
        strcat_check(new_property, " Slay Evil", LPROP);
}
static void add_slay_animal(artifact_type *a_ptr)
{
        if (a_ptr->flags1 & TR1_SLAY_ANIMAL) return;
        a_ptr->flags1 |= TR1_SLAY_ANIMAL;
        strcat_check(new_property, " Slay Animals", LPROP);
}
static void add_slay_undead(artifact_type *a_ptr);
static void add_slay_demon(artifact_type *a_ptr);
static void add_slay_undead(artifact_type *a_ptr)
{
        if (rand_int(2) == 0) add_slay_demon(a_ptr);
        if (a_ptr->flags1 & TR1_SLAY_UNDEAD) return;
        a_ptr->flags1 |= TR1_SLAY_UNDEAD;
        strcat_check(new_property, " Slay Undeads", LPROP);
}
static void add_slay_demon(artifact_type *a_ptr)
{
        if (rand_int(2) == 0) add_slay_undead(a_ptr);
        if (a_ptr->flags1 & TR1_SLAY_DEMON) return;
        a_ptr->flags1 |= TR1_SLAY_DEMON;
        strcat_check(new_property, " Slay Demons", LPROP);
}
static void add_uncommon_specific(artifact_type *a_ptr)
{
		int r = rand_int(100);
		switch (a_ptr->tval)
		{
			case TV_BOW:
			{
                                add_might(a_ptr);
				break;
			}
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			{
				if (r < 3) add_str(a_ptr);
				else if (r < 5) add_dex(a_ptr);
				else if (r < 13) add_acid(a_ptr);
				else if (r < 21) add_elec(a_ptr);
				else if (r < 33) add_fire(a_ptr);
				else if (r < 45) add_cold(a_ptr);
				else if (r < 55) add_slay_dragon(a_ptr);
				else if (r < 67) add_slay_evil(a_ptr);
				else if (r < 72) add_slay_animal(a_ptr);
				else if (r < 82) add_slay_undead(a_ptr);
				else if (r < 87) add_slay_demon(a_ptr);
				else if (r < 92) add_see_invis(a_ptr);
				else add_to_a(a_ptr, 3 + rand_int(3));
				break;
			}
			case TV_BOOTS:
			{
			        add_stealth(a_ptr);
                                break;
			}
			case TV_GLOVES:
			{
				if (r < 25) add_free_act(a_ptr);
				else if (r < 50) add_dex(a_ptr);
				else add_to_h_to_d(a_ptr, 2 + rand_int(3));
				break;
			}
			case TV_HELM:
			case TV_CROWN:
			{
				if (r < 40) add_wis(a_ptr);
				else if (r < 80) add_int(a_ptr);
				else add_chr(a_ptr);
				break;
			}
			case TV_SHIELD:
			{
                                add_to_a(a_ptr, 10);
				break;
			}
			case TV_CLOAK:
			{
                                add_stealth(a_ptr);
				break;
			}
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			{
				if (r < 40) add_stealth(a_ptr);
				else add_con(a_ptr);
				break;
			}
			case TV_RING:
			case TV_AMULET:
			case TV_LITE:
                        {
				if (r < 60) add_to_a(a_ptr, 3 + rand_int(3) + rand_int(3) + rand_int(3));
				else add_to_h_to_d(a_ptr, 3 + rand_int(3) + rand_int(3) + rand_int(3));
                                break;
                        }
		}/*switch (a_ptr->tval)*/
}
static void add_shots(artifact_type *a_ptr)
{
	do_pval(a_ptr);
        if (a_ptr->flags1 & TR1_SHOTS) return;
        a_ptr->flags1 |= TR1_SHOTS;
        strcat_check(new_property, " Shots", LPROP);
}
static void add_dd(artifact_type *a_ptr, int plus)
{
        /* Alex: it is impossible to overflow 80-char array by +5d5 or so */
        char buf[80];
        char *t = buf;
        int old_dd = a_ptr->dd;
        a_ptr->dd += plus;
        if (a_ptr->dd > 9) a_ptr->dd = 9;
        plus = a_ptr->dd - old_dd;

        if (plus<=0) return;

        object_desc_str_macro(t, " +");
        object_desc_num_macro(t, plus);
        object_desc_chr_macro(t, 'd');
        object_desc_num_macro(t, a_ptr->ds);
        object_desc_chr_macro(t, '\0');
        strcat_check(new_property, buf, LPROP);
}
static void add_ac(artifact_type *a_ptr, int plus)
{
        /* Alex: it is impossible to overflow 80-char array by [+xxx,+0] */
        char buf[80];
        char *t = buf;
	a_ptr->ac += plus;
        object_desc_str_macro(t, " [");
        object_desc_int_macro(t, plus);
        object_desc_str_macro(t, ",+0]");
        object_desc_chr_macro(t, '\0');
        strcat_check(new_property, buf, LPROP);
}
static void remove_to_h_pen(artifact_type *a_ptr)
{
        /* Alex: it is impossible to overflow 80-char array by (+xxx,+0) */
        char buf[80];
        char *t = buf;
	int tmp = a_ptr->to_h;
	if (a_ptr->to_h<0) a_ptr->to_h++;
	while(rand_int(2) && a_ptr->to_h<0) a_ptr->to_h++;
	tmp = a_ptr->to_h - tmp;
	if (!tmp) return;
        object_desc_str_macro(t, " (");
        object_desc_int_macro(t, tmp);
        object_desc_str_macro(t, ",+0)");
        object_desc_chr_macro(t, '\0');
        strcat_check(new_property, buf, LPROP);
}
static void add_kill_dragon(artifact_type *a_ptr)
{
        if (a_ptr->flags1 & TR1_KILL_DRAGON) return;
        a_ptr->flags1 |= TR1_KILL_DRAGON;
        strcat_check(new_property, " Kill Dragons", LPROP);
}
static void add_kill_undead(artifact_type *a_ptr)
{
        if (a_ptr->flags1 & TR1_KILL_UNDEAD) return;
        a_ptr->flags1 |= TR1_KILL_UNDEAD;
        strcat_check(new_property, " Kill Undeads", LPROP);
}
static void add_kill_demon(artifact_type *a_ptr)
{
        if (a_ptr->flags1 & TR1_KILL_DEMON) return;
        a_ptr->flags1 |= TR1_KILL_DEMON;
        strcat_check(new_property, " Kill Demons", LPROP);
}
static void add_rare_specific(artifact_type *a_ptr)
{
		int r = rand_int(100);
		switch (a_ptr->tval)
		{
			case TV_BOW:
			{
				add_shots(a_ptr);
				break;
			}
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			{
				if (r < 30) add_dd(a_ptr, 1 + rand_int(2) + rand_int(2) + rand_int(2));
				else if (r < 55) add_kill_dragon(a_ptr);
				else if (r < 80) add_kill_undead(a_ptr);
				else add_kill_demon(a_ptr);
				break;
			}
			case TV_BOOTS:
			{
                                if (r<10) add_ac(a_ptr, 5);
                                else 
                                {
					if (a_ptr->pval >= 0) add_speed(a_ptr, 3 + rand_int(8));
                                        else add_speed(a_ptr, 0);
                                }
				break;
			}
			case TV_GLOVES:
			{
                                if (r<10) add_ac(a_ptr, 5);
                                else if (r<70) add_to_a(a_ptr, 10);
                                else add_to_h_to_d(a_ptr, 10);
				break;
			}
			case TV_HELM:
			case TV_CROWN:
			{
                                if (r<10) add_ac(a_ptr, 5);
				else if (r < 50) add_telepathy(a_ptr);
				else add_to_a(a_ptr, 10);
				break;
			}
			case TV_SHIELD:
			case TV_CLOAK:
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			{
                                if (r<10) add_ac(a_ptr, 5);
			        else if (r<18) add_hold_life(a_ptr);
			        else if (r<26) add_res_confu(a_ptr);
			        else if (r<34) add_res_sound(a_ptr);
			        else if (r<42) add_res_shard(a_ptr);
			        else if (r<50) add_res_nexus(a_ptr);
			        else if (r<58) add_res_chaos(a_ptr);
                                else if (r<66) add_resistance(a_ptr);
                                else if (r<74) add_sust_all(a_ptr);
                                else if (r<80) remove_to_h_pen(a_ptr);
                                else add_to_a(a_ptr, 10);
				break;
			}
			case TV_RING:
			case TV_AMULET:
                        {
			        if (r<8) add_hold_life(a_ptr);
			        else if (r<16) add_res_confu(a_ptr);
			        else if (r<24) add_res_sound(a_ptr);
			        else if (r<32) add_res_shard(a_ptr);
			        else if (r<40) add_res_nexus(a_ptr);
			        else if (r<48) add_res_chaos(a_ptr);
                                else if (r<56) add_resistance(a_ptr);
                                else if (r<64) add_sust_all(a_ptr);
                                else if (r<82) add_to_a(a_ptr, 10);
				else add_to_h_to_d(a_ptr, 10);
				break;
                        }
			case TV_LITE:
                        {
				if (r < 50) add_telepathy(a_ptr);
				else add_to_a(a_ptr, 10);
				break;
                        }
		}/*switch (a_ptr->tval)*/
}
static void add_might_shots(artifact_type *a_ptr)
{
	do_pval(a_ptr);
        if ((a_ptr->flags1 & TR1_MIGHT) &&
             (a_ptr->flags1 & TR1_SHOTS))
                return;
        a_ptr->flags1 |= TR1_MIGHT;
        a_ptr->flags1 |= TR1_SHOTS;
        strcat_check(new_property, " Might and Shots", LPROP);
}
static void add_impact(artifact_type *a_ptr)
{
        if (a_ptr->flags3 & TR3_IMPACT) return;
        a_ptr->flags3 |= TR3_IMPACT;
        strcat_check(new_property, " Earthquake", LPROP);
}
static void add_kill_all(artifact_type *a_ptr)
{
        if ((a_ptr->flags1 & TR1_KILL_DRAGON) &&
             (a_ptr->flags1 & TR1_KILL_UNDEAD) &&
             (a_ptr->flags1 & TR1_KILL_DEMON))
                return;
        a_ptr->flags1 |= TR1_KILL_DRAGON;
        a_ptr->flags1 |= TR1_KILL_UNDEAD;
        a_ptr->flags1 |= TR1_KILL_DEMON;
        strcat_check(new_property, " Kill Dragons, Undeads, and Demons", LPROP);
}
static void add_very_rare_specific(artifact_type *a_ptr)
{
		int r = rand_int(100);
		switch (a_ptr->tval)
		{
			case TV_BOW:
			{
                                if (r<50) add_might_shots(a_ptr);
				else add_to_h_to_d(a_ptr, 10);
				break;
			}
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			{
                                if (r<10 && a_ptr->tval == TV_DIGGING) add_impact(a_ptr);
				else if (r< 20) add_dd(a_ptr, 9);
				else if (r < 50) add_kill_all(a_ptr);
				else if (r < 80)
				{
					if (a_ptr->pval < 0) break;
					add_blow(a_ptr);
				}
				else if (r < 90) add_to_h_to_d(a_ptr, 10);
				else add_to_a(a_ptr, 10);
				break;
			}
			case TV_BOOTS:
			{
				if (r < 70)
				{
					if (a_ptr->pval < 0) break;
					add_speed(a_ptr, 10);
				}
				else add_to_a(a_ptr, 20);
				break;
			}
			case TV_GLOVES:
			{
				if (r < 20)
				{
					if (a_ptr->pval < 0) break;
					add_speed(a_ptr, 0);
				}
				else add_to_a(a_ptr, 20);
				break;
			}
			case TV_HELM:
			case TV_CROWN:
			case TV_SHIELD:
			case TV_CLOAK:
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
			case TV_DRAG_ARMOR:
			case TV_RING:
			case TV_AMULET:
			case TV_LITE:
			{
			        if (r < 10) add_im_acid(a_ptr);
			        else if (r < 20) add_im_elec(a_ptr);
			        else if (r < 30) add_im_fire(a_ptr);
			        else if (r < 40) add_im_cold(a_ptr);
			        else if (r < 50) add_res_nether(a_ptr);
			        else if (r < 60) add_res_disen(a_ptr);
				else add_to_a(a_ptr, 20);
				break;
			}
		}/*switch (a_ptr->tval)*/
}
/*
 * Randomly select an extra ability to be added to the artifact in question.
 * XXX - This function is way too large.
 *
 * Alex: tends to add rare abilities to powerful artifacts.
 * Delta is the difference between required power and current power
 *
 * ToDo: Add the KILL_UNDEAD and KILL_DEMON flags.
 * Alex: done
 */
static void add_ability(artifact_type *a_ptr, int delta)
{
        int r, limit, rarity;

        if (!rand_int(10) && a_ptr->pval)
        {
                do_pval(a_ptr);
                return;
        }

        /* Alex: weapon should have more specific abilities */
        switch (a_ptr->tval)
        {
                case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
                case TV_BOW:
                        limit = 8;
                        break;
                default:
                        limit = 6;
        }
        r = rand_int(10);
        if (delta<ART_R_1)
        {
                if (r<6) rarity = 0;/*The most probable*/
                else rarity = 1;
        }
        else if (delta <ART_R_2)
        {
                if (r<1) rarity = 0;
                else if (r<7) rarity = 1;/*The most probable*/
                else rarity = 2;
        }
        else if (delta <ART_R_3)
        {
                if (r<1) rarity = 1;
                else if (r<7) rarity = 2;/*The most probable*/
                else rarity = 3;
        }
        else
        {
                if (r<2) rarity = 2;
                else rarity = 3;/*The most probable*/
        }

	r = rand_int(10);
	if (r < limit)		/* Pick something dependent on item type. */
	{
                switch (rarity)
                {
                        case 0: add_common_specific(a_ptr); break;
                        case 1: add_uncommon_specific(a_ptr); break;
                        case 2: add_rare_specific(a_ptr); break;
                        case 3: add_very_rare_specific(a_ptr); break;
                }
	}
	else			/* Pick something universally useful. */
	{
                switch (rarity)
                {
                        case 0: add_common_ability(a_ptr); break;
                        case 1: add_uncommon_ability(a_ptr); break;
                        case 2: add_rare_ability(a_ptr); break;
                        case 3: add_very_rare_ability(a_ptr); break;
                }
	}

	/* Now remove contradictory or redundant powers. */
	remove_contradictory(a_ptr);
}


/*
 * Make it bad, or if it's already bad, make it worse!
 */
static void do_curse(artifact_type *a_ptr)
{
	if (rand_int(3) == 0)
		a_ptr->flags3 |= TR3_AGGRAVATE;
	if (rand_int(5) == 0)
		a_ptr->flags3 |= TR3_DRAIN_EXP;
	if (rand_int(7) == 0)
		a_ptr->flags3 |= TR3_TELEPORT;

	if ((a_ptr->pval > 0) && (rand_int(2) == 0))
		a_ptr->pval = -a_ptr->pval;
	if ((a_ptr->to_a > 0) && (rand_int(2) == 0))
		a_ptr->to_a = -a_ptr->to_a;
	if ((a_ptr->to_h > 0) && (rand_int(2) == 0))
		a_ptr->to_h = -a_ptr->to_h;
	if ((a_ptr->to_d > 0) && (rand_int(4) == 0))
		a_ptr->to_d = -a_ptr->to_d;

	if (a_ptr->flags3 & TR3_LIGHT_CURSE)
	{
		if (rand_int(2) == 0) a_ptr->flags3 |= TR3_HEAVY_CURSE;
		return;
	}

	a_ptr->flags3 |= TR3_LIGHT_CURSE;

	if (rand_int(4) == 0)
		a_ptr->flags3 |= TR3_HEAVY_CURSE;
}


static void scramble_artifact(int a_idx, const object_type* base)
{
	artifact_type *a_ptr = &a_info[a_idx];
	u32b activates = a_ptr->flags3 & TR3_ACTIVATE;
	s32b power;
	int tries;
	s32b ap;
	bool curse_me = FALSE;
	bool aggravate_me = FALSE;

	/* XXX XXX XXX Special cases -- don't randomize these! */
	if ((a_idx == ART_POWER) ||
	    (a_idx == ART_GROND) ||
	    (a_idx == ART_MORGOTH))
		return;

	/* Skip unused artifacts */
	if (a_ptr->tval == 0) return;

	/* Evaluate the original artifact to determine the power level. */
	power = artifact_power(a_idx, a_ptr);
	if (power < 0)
        {
                curse_me = TRUE;
                cursed_art = TRUE;/* Alex: for do_pval()*/
        }
        else
                cursed_art = FALSE;/* Alex: for do_pval()*/

	if (randart_verbose)
		msg_format("Artifact %d: power = %d", a_idx, power);

	/* Really powerful items should aggravate. */
	if (power > 100)
	{
		if (rand_int(100) < (power - 100) * 3)
			aggravate_me = TRUE;
	}

	if (a_idx >= ART_MIN_NORMAL)
	{
		/*
		 * Normal artifact - choose a random base item type.  Not too
		 * powerful, so we'll have to add something to it.  Not too
		 * weak, for the opposite reason.
		 */
		int count = 0;
		s32b ap2;

		do
		{
			choose_item(a_idx, base);
			ap2 = artifact_power(a_idx, a_ptr);
			count++;
		} while ((count < MAX_TRIES) &&
			   ((ap2 > (power * 8) / 10 + 1) ||
			    (ap2 < (power / 10))));
	}
	else
	{
		/*
		 * Special artifact (light source, ring, or amulet).
		 * Clear the following fields; leave the rest alone.
		 */
		a_ptr->pval = 0;
		a_ptr->to_h = a_ptr->to_d = a_ptr->to_a = 0;
		a_ptr->flags1 = a_ptr->flags2 = 0;

		/* Artifacts ignore everything */
		a_ptr->flags3 = (TR3_IGNORE_MASK);
	}

	/* First draft: add two abilities, then curse it three times. */
	if (curse_me)
	{
                strcpy(new_property, "");
		add_ability(a_ptr, power - artifact_power(a_idx, a_ptr));
                strcpy(new_property, "");
		add_ability(a_ptr, power - artifact_power(a_idx, a_ptr));
		do_curse(a_ptr);
		do_curse(a_ptr);
		do_curse(a_ptr);
		remove_contradictory(a_ptr);
		ap = artifact_power(a_idx, a_ptr);
	}
	else
	{
		/*
		 * Select a random set of abilities which roughly matches the
		 * original's in terms of overall power/usefulness.
		 */
		for (tries = 0; tries < MAX_TRIES; tries++)
		{
			artifact_type a_old;

			/* Copy artifact info temporarily. */
			a_old = *a_ptr;
                        strcpy(new_property, "");
			add_ability(a_ptr, power - artifact_power(a_idx, a_ptr));
			ap = artifact_power(a_idx, a_ptr);

			if (ap > (power * 11) / 10 + 1)
			{
				/* too powerful -- put it back */
				*a_ptr = a_old;
				continue;
			}
			else if (ap >= (power * 9) / 10)	/* just right */
			{
				break;
			}

			/* Stop if we're going negative, so we don't overload
			   the artifact with great powers to compensate. */
			else if ((ap < 0) && (ap < (-(power * 1)) / 10))
			{
				break;
			}
		}		/* end of power selection */

		if (aggravate_me)
		{
			a_ptr->flags3 |= TR3_AGGRAVATE;
			remove_contradictory(a_ptr);
			ap = artifact_power(a_idx, a_ptr);
		}
	}

	a_ptr->cost = ap * 1000L;

	if (a_ptr->cost < 0) a_ptr->cost = 0;

#if 0
	/* One last hack: if the artifact is very powerful, raise the rarity.
	   This compensates for artifacts like (original) Bladeturner, which
	   have low artifact rarities but came from extremely-rare base
	   kinds. */
	if ((ap > 0) && ((ap / 8) > a_ptr->rarity))
		a_ptr->rarity = ap / 8;
#endif /* 0 */

	/* Restore some flags */
	if (activates) a_ptr->flags3 |= TR3_ACTIVATE;
	if (a_idx < ART_MIN_NORMAL) a_ptr->flags3 |= TR3_INSTA_ART;

	/*
	 * Add TR3_HIDE_TYPE to all artifacts with nonzero pval because we're
	 * too lazy to find out which ones need it and which ones don't.
	 */
	if (a_ptr->pval)
		a_ptr->flags3 |= TR3_HIDE_TYPE;

        last_power = ap;
}


/*
 * Return TRUE if the whole set of random artifacts meets certain criteria.
 */
static bool artifacts_acceptable(void)
{
	int swords = 5, polearms = 5, blunts = 5, bows = 3;
	int bodies = 5, shields = 3, cloaks = 3, hats = 4;
	int gloves = 4, boots = 4;
	int i;

	for (i = ART_MIN_NORMAL; i < z_info->a_max; i++)
	{
		switch (a_info[i].tval)
		{
			case TV_SWORD:
				swords--; break;
			case TV_POLEARM:
				polearms--; break;
			case TV_HAFTED:
				blunts--; break;
			case TV_BOW:
				bows--; break;
			case TV_SOFT_ARMOR:
			case TV_HARD_ARMOR:
				bodies--; break;
			case TV_SHIELD:
				shields--; break;
			case TV_CLOAK:
				cloaks--; break;
			case TV_HELM:
			case TV_CROWN:
				hats--; break;
			case TV_GLOVES:
				gloves--; break;
			case TV_BOOTS:
				boots--; break;
		}
	}

	if (swords > 0 || polearms > 0 || blunts > 0 || bows > 0 ||
	    bodies > 0 || shields > 0 || cloaks > 0 || hats > 0 ||
	    gloves > 0 || boots > 0)
	{
		if (randart_verbose)
		{
			char types[256];
			sprintf(types, "%s%s%s%s%s%s%s%s%s%s",
				swords > 0 ? " swords" : "",
				polearms > 0 ? " polearms" : "",
				blunts > 0 ? " blunts" : "",
				bows > 0 ? " bows" : "",
				bodies > 0 ? " body-armors" : "",
				shields > 0 ? " shields" : "",
				cloaks > 0 ? " cloaks" : "",
				hats > 0 ? " hats" : "",
				gloves > 0 ? " gloves" : "",
				boots > 0 ? " boots" : "");
			msg_format("Restarting generation process: not enough%s",
				types);
		}

		/* Not acceptable */
		return (FALSE);
	}
	else
	{
		/* Acceptable */
		return (TRUE);
	}
}


static errr scramble(void)
{
	/* Allocate the "kinds" array */
	C_MAKE(kinds, z_info->a_max, s16b);

	while (1)
	{
		int a_idx;

		/* Generate all the artifacts. */
		for (a_idx = 1; a_idx < z_info->a_max; a_idx++)
		{
			scramble_artifact(a_idx, NULL);
		}

		if (artifacts_acceptable()) break;
	}

	/* Free the "kinds" array */
	FREE(kinds);
        kinds = NULL;

	/* Success */
	return (0);
}


static errr do_randart_aux(bool full)
{
	errr result;

	/* Generate random names */
	if ((result = init_names()) != 0) return (result);

	if (full)
	{
		/* Randomize the artifacts */
		if ((result = scramble()) != 0) return (result);
	}

	/* Success */
	return (0);
}


/*
 * Randomize the artifacts
 *
 * The full flag toggles between just randomizing the names and
 * complete randomization of the artifacts.
 */
errr do_randart(u32b randart_seed, bool full)
{
	errr err;

	/* Prepare to use the Angband "simple" RNG. */
	Rand_value = randart_seed;
	Rand_quick = TRUE;

	/* Generate the random artifact (names) */
	err = do_randart_aux(full);

	/* When done, resume use of the Angband "complex" RNG. */
	Rand_quick = FALSE;

	return (err);
}

/* Alex */
/* Adds memory for n artifacts; memory is not wiped */
/* Returns TRUE if success */
bool realloc_art(int n){
        int new_size = z_info->a_max + n;
        artifact_type* a = realloc(a_info, sizeof(artifact_type)*new_size);
        s16b* k = realloc(kinds, sizeof(s16b)*new_size);
        if (a && k){
                int i;
                z_info->a_max = new_size;
                a_head.info_ptr = a;
                a_head.info_num = z_info->a_max;
                a_head.info_size = z_info->a_max * sizeof(artifact_type);
                a_info = a;
                kinds = k;
                for (i = z_info->a_max - n; i< z_info->a_max; i++)
                        kinds[i] = 0;
                return TRUE;
        }
        else
                return FALSE;
}

/* Returns TRUE if success */
bool add_artifact(const artifact_type* a_ptr /*, const char* name*/)
{
        int name_index = 0;
        int text_index = rand_int(z_info->a_max-1) + 1;
        artifact_type* a;
        /*int name_len = 0;
        char* names = 0;*/

        /* realloc() is supposed to reserve more than one to reduce number of reallocations */
        if (!realloc_art(1))
                return FALSE;
        a = &a_info[z_info->a_max-1];
        *a = *a_ptr;
        /*COPY(&a_info[z_info->a_max-1], a_ptr, artifact_type);*/
        name_index = rand_int(names_list_len - 10);

        while(names_list[name_index] != '\n')
                name_index++;
        if (name_index == names_list_len - 1)
                /*It is impossible - "Yavanna" will be the last name*/
                a->name = 0;
        else
                a->name = name_index + 1;

        a->text = a_info[text_index].text;

        a->cur_num = 0;
        a->force_depth = 0;

        if (!a->tval)
                message_format(MSG_KILL, 0, "ZERO ARTIFACT TVAL!");

        return TRUE;
}

/* Alex: adds extra random artifact based on old artifact number a_idx */
/* Not used now */
bool add_random_art(int a_idx, const object_type* base){
        artifact_type a_old;
        artifact_type a_new;

	if ((a_idx == 1) ||             /* Alex: The Phial is the same */
	    (a_idx == ART_POWER) ||
	    (a_idx == ART_GROND) ||
	    (a_idx == ART_MORGOTH))
		return FALSE;

        a_old = a_info[a_idx];
        Rand_quick = TRUE;
        if (base->tval)
                scramble_artifact(a_idx, base);
        else
                scramble_artifact(a_idx, NULL);
	Rand_quick = FALSE;
        a_new = a_info[a_idx];
        a_info[a_idx] = a_old;

        msg_format("Artifact base: %d!", a_idx);

        return add_artifact(&a_new);
}

void init_randart(){
        if (!kinds)
        {
	        /* Allocate the "kinds" array */
                /* FREE in done_randart() */
	        C_MAKE(kinds, z_info->a_max, s16b);
        }
        if (!names_list_len) names_list_len = strlen(names_list);
        if (!new_art_names)
        {
                char* cp;
                new_art_names = malloc(names_list_len+1);
                strcpy(new_art_names, names_list);
                cp = new_art_names;
                *cp = toupper(*cp);
                while (*cp)
                {
                        if (*cp == '\n')
                        {
                                *cp = 0;
                                cp++;
                                if (*cp)
                                        *cp = toupper(*cp);
                        }
                        else
                                cp++;
                }
        }
}

void done_randart()
{
        if (kinds)
        {
                FREE(kinds);
                kinds = NULL;
        }
        if (new_art_names)
        {
                free(new_art_names);
                new_art_names = NULL;
        }
}

/*base must not be NULL*/
void make_base(artifact_type *a_ptr, const object_type* base)
{
        s16b k_idx;
	object_kind *k_ptr;

        k_idx = lookup_kind(base->tval, base->sval);
        k_ptr = &k_info[k_idx];

        /* Create artifact based on base */
	a_ptr->tval = base->tval;
	a_ptr->sval = base->sval;
	a_ptr->pval = base->pval;
	a_ptr->to_h = base->to_h;
	a_ptr->to_d = base->to_d;
	a_ptr->to_a = base->to_a;
	a_ptr->ac = base->ac;
	a_ptr->dd = base->dd;
	a_ptr->ds = base->ds;
	a_ptr->weight = base->weight;
	a_ptr->flags1 = k_ptr->flags1;
	a_ptr->flags2 = k_ptr->flags2;
	a_ptr->flags3 = k_ptr->flags3;

	switch (base->xtra1)
	{
		case OBJECT_XTRA_TYPE_SUSTAIN:
		{
			/* OBJECT_XTRA_WHAT_SUSTAIN == 2 */
			a_ptr->flags2 |= (OBJECT_XTRA_BASE_SUSTAIN << base->xtra2);
			break;
		}

		case OBJECT_XTRA_TYPE_RESIST:
		{
			/* OBJECT_XTRA_WHAT_RESIST == 2 */
			a_ptr->flags2 |= (OBJECT_XTRA_BASE_RESIST << base->xtra2);
			break;
		}

		case OBJECT_XTRA_TYPE_POWER:
		{
			/* OBJECT_XTRA_WHAT_POWER == 3 */
			a_ptr->flags3 |= (OBJECT_XTRA_BASE_POWER << base->xtra2);
			break;
		}
	}

        if (base->name2)
        {
                ego_item_type* e_ptr = &e_info[base->name2];
                a_ptr->level = e_ptr->level;
                a_ptr->rarity = e_ptr->rarity;
        	a_ptr->flags1 |= e_ptr->flags1;
	        a_ptr->flags2 |= e_ptr->flags2;
	        a_ptr->flags3 |= e_ptr->flags3;
        }
        else
        {
        	a_ptr->level = k_ptr->level;
	        a_ptr->rarity = k_ptr->chance[0];
        }

	/* Artifacts ignore everything */
	a_ptr->flags3 |= TR3_IGNORE_MASK;

	/* Assign basic stats to the artifact based on its artifact level. */
                        /*
			 * Make sure armor gets some resists!  Hard body armor
			 * is generally high-level stuff, with good ac and
			 * to_a.  That sucks up all the points....
			 */
        switch (a_ptr->tval)
	{
                case TV_SOFT_ARMOR:
	        case TV_HARD_ARMOR:
	                if (rand_int(2) == 0) a_ptr->flags2 |= TR2_RES_ACID;
		        if (rand_int(2) == 0) a_ptr->flags2 |= TR2_RES_ELEC;
		        if (rand_int(2) == 0) a_ptr->flags2 |= TR2_RES_COLD;
		        if (rand_int(2) == 0) a_ptr->flags2 |= TR2_RES_FIRE;
		        break;
        }
        if (a_ptr->rarity <1)
                a_ptr->rarity = 1;
}

/* Alex: adds extra random artifact begining from base object) */
bool add_new_art(int power, const object_type* base, bool player_choose)
{
        artifact_type art_body;
        artifact_type* a_ptr = &art_body;
        s32b initial_cost = 0;

        if (!base)
        {
                last_power = 0;
                return FALSE;
        }

        initial_cost = object_value_real(base);

        make_base(a_ptr, base);

        if (!enchant_artifact(a_ptr, power, player_choose))
                return FALSE;

	a_ptr->cost += initial_cost;

        if (!rand_int(3))
                a_ptr->flags3 |= TR3_ACTIVATE; /*This flag might be set in make_base() */


        /* Note that ACT_MAX means "activation as a base object kind"
         * (see script object.lua)
        */
        if (a_ptr->flags3 & TR3_ACTIVATE)
        {
                a_ptr->time = 0;
                a_ptr->randtime = 0;
                switch (a_ptr->tval)
                {
                case TV_RING:
                        {
                                switch (a_ptr->sval)
                                {
                                case SV_RING_ACID:
                                case SV_RING_FLAMES:
                                case SV_RING_ICE:
                                case SV_RING_LIGHTNING:
                                        a_ptr->activation = ACT_MAX;
                                        a_ptr->time = 25;
                                        a_ptr->randtime = 25;
                                        break;
                                default:
                                        a_ptr->activation = (byte)rand_int(ACT_MAX);
                                }/*TV_RING, switch (a_ptr->sval)*/
                                break;
                        }
                case TV_LITE:
                        {
                                if (rand_int(3))
                                {
                                        a_ptr->activation = ACT_ILLUMINATION;
                                        a_ptr->time = (u16b)rand_int(11);
                                        a_ptr->randtime = 10;
                                }
                                else
                                        a_ptr->activation = (byte)rand_int(ACT_MAX);
                                break;
                        }
                case TV_DRAG_ARMOR:
                                a_ptr->activation = ACT_MAX;
                                a_ptr->time = 200;
                                a_ptr->randtime = 200;
                                break;
                default:
                        a_ptr->activation = (byte)rand_int(ACT_MAX);
                }/*switch (a_ptr->tval)*/
                if (!a_ptr->time && !a_ptr->randtime)
                {
                        a_ptr->time = (u16b)(10*(4 + rand_int(10) + rand_int(10) + rand_int(10) + rand_int(10)));
                        a_ptr->randtime = (u16b)(10*rand_int(a_ptr->time/10));
                }

        }
        else
        {
                a_ptr->activation = ACT_MAX;
                a_ptr->time = a_ptr->randtime = 0;
        }

        if (is_special(a_ptr->tval))
                a_ptr->flags3 |= TR3_INSTA_ART;

        a_ptr->cur_num = 0;
        a_ptr->force_depth = 0;
        a_ptr->max_num = 1;
        
        return add_artifact(a_ptr);
}

/* Alex: enchant an existing artifact */
bool enchant_artifact(artifact_type* a_ptr, int power, bool player_choose)
{
        artifact_type art_try;
        int original_power = 0;
        int prev_power = 0;

        if (power >= 0)
        {
                cursed_art = FALSE;
                while (!rand_int(GREAT_OBJ))
                        power += 50;
        }
        else
                cursed_art = TRUE;

        if (power >= 0)
        {
                int i;


                original_power = prev_power = artifact_power(0, a_ptr);
                if (original_power > power)
		{
			last_power = original_power;
                        return FALSE;
		}

                art_try = *a_ptr;
                if (player_choose) msg_format("Maximum artifact power: %d", power);

                for (i = 0; i < MAX_TRIES; i++)
                {
                        if (player_choose) msg_format("Try: %d of %d.", i + 1, MAX_TRIES);
                        while(1)
                        {
                                bool accept = FALSE;
                                int old_pval = art_try.pval;
                                strcpy(new_property, "");
                                add_ability(&art_try, power - prev_power);
                                last_power = artifact_power(0, &art_try);
                                if (last_power <= power)
                                {
                                        if (player_choose)
                                        {
                                                if (last_power != prev_power)
                                                {
                                                        /*  Something changes */
                                                        if (old_pval != art_try.pval) msg_format("New pval: %d.", art_try.pval);
							else msg_format("pval: %d.", art_try.pval);
                                                        if (new_property[0] != '\0') msg_format("New ablity:%s.", new_property);
                                                        msg_format("New power: %d (%+d) of %d.", last_power, last_power-prev_power, power);
                                                }
                                                if (last_power > prev_power)
                                                {
                                                        /* Good change - ask player */
                                                        char out_val[2] = "n";
                                	                if (get_string("Do you accept new ability? (y/n)", out_val, 2) && out_val[0] == 'y')
                                                                accept = TRUE;
                                                        else
                                                                accept = FALSE;
                                                }
                                                /* Automatically accept bad changes */
                                                else if (last_power < prev_power)
                                                                accept = TRUE;
                                                        else
                                                                accept = FALSE;
                                        }
                                        else
                                                accept = TRUE;
                                        if (accept)
                                        {
                                                *a_ptr = art_try;
                                                prev_power = last_power;
                                        }
                                        else
                                        {
                                                art_try = *a_ptr;
                                        }
                                }
                                else
                                {
                                        art_try = *a_ptr;
                                        break;
                                }
                        }
                        prev_power = artifact_power(0, a_ptr);
                        if (prev_power == power)
                                break;
                }
		last_power = prev_power;
                if (prev_power > power)
                        return FALSE;
        }
        else
        {
                original_power = last_power = artifact_power(0, a_ptr);
                while(last_power > power)
                {
                        strcpy(new_property, "");
                        add_ability(a_ptr, power - last_power);
                        do_curse(a_ptr);
                        remove_contradictory(a_ptr);
                        last_power = artifact_power(0, a_ptr);
                }
                if (!rand_int(4))
		{
			a_ptr->flags3 |= TR3_HEAVY_CURSE;
			if (!rand_int(4))
				a_ptr->flags3 |= TR3_PERMA_CURSE;
		}
        }

	a_ptr->cost = last_power * 1000L;

	if (a_ptr->cost < 0) a_ptr->cost = 0;

	if (abs(last_power / 8) > a_ptr->rarity)
		a_ptr->rarity = abs(last_power / 8);

	/*
	 * Add TR3_HIDE_TYPE to all artifacts with nonzero pval because we're
	 * too lazy to find out which ones need it and which ones don't.
	 */
	if (a_ptr->pval)
		a_ptr->flags3 |= TR3_HIDE_TYPE;

        return original_power != last_power;
}

#else /* GJW_RANDART */

#ifdef MACINTOSH
static int i = 0;
#endif /* MACINTOSH */

#endif /* GJW_RANDART */
