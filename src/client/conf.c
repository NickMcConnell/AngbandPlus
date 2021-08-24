/*
 * File: conf.c
 * Purpose: INI file configuration
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2020 MAngband and PWMAngband Developers
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */


#include "c-angband.h"


/*
 * Client config file handler
 */
static char config_name[MSG_LEN];  /* Config filename */


/*
 * WINDOWS specific code
 */


#ifdef WINDOWS
void conf_init(void* param)
{
    char path[MSG_LEN];
    HINSTANCE hInstance = param;

    /* Search for file in user directory */
    if (GetEnvironmentVariable("USERPROFILE", path, sizeof(path)))
    {
        my_strcat(path, "\\mangclient.ini", sizeof(path));

        /* Ok */
        if (file_exists(path))
        {
            my_strcpy(config_name, path, sizeof(config_name));
            return;
        }
    }

    /* Get full path to executable */
    GetModuleFileName(hInstance, path, sizeof(path));
    my_strcpy(path + strlen(path) - 4, ".ini", 5);
    my_strcpy(config_name, path, sizeof(config_name));
}


void conf_save(void)
{
}


void conf_timer(int ticks)
{
}


bool conf_section_exists(const char *section)
{
    char sections[MSG_LEN];
    int n;
    size_t i;

    n = GetPrivateProfileSectionNames(sections, MSG_LEN, config_name);
    if (n != MSG_LEN - 2)
    {
        for (i = 0; sections[i]; i += (strlen(&sections[i]) + 1))
        {
            if (!my_stricmp(&sections[i], section)) return true;
        }
    }

    return false;
}


const char *conf_get_string(const char *section, const char *name, const char *default_value)
{
    static char value[100];

    GetPrivateProfileString(section, name, default_value, value, 100, config_name);
    return &value[0];
}


s32b conf_get_int(const char *section, const char *name, s32b default_value)
{
    return GetPrivateProfileInt(section, name, default_value, config_name);
}


void conf_set_string(const char *section, const char *name, const char *value)
{
    WritePrivateProfileString(section, name, value, config_name);
}


void conf_set_int(const char *section, const char *name, s32b value)
{
    char s_value[100];

    strnfmt(s_value, sizeof(s_value), "%" PRId32, value);
    WritePrivateProfileString(section, name, s_value, config_name);
}


/* Hack -- append section */
void conf_append_section(const char *sectionFrom, const char *sectionTo, const char *filename)
{
    char keys[2 * MSG_LEN];
    char value[MSG_LEN];
    int n;
    size_t i;

    /* Get all keys */
    n = GetPrivateProfileString(sectionTo, NULL, NULL, keys, 2 * MSG_LEN, filename);
    if (n != 2 * MSG_LEN - 2)
    {
        for (i = 0; keys[i]; i += (strlen(&keys[i]) + 1))
        {
            /* Extract key */
            GetPrivateProfileString(sectionFrom, &keys[i], "", value, sizeof(value),
                filename);

            /* Hack -- append key to original config */
            value[100] = '\0'; /* FIXME: change "strings" len */
            conf_set_string(sectionTo, &keys[i], value);
        }
    }
}


bool conf_exists(void)
{
    return file_exists(config_name);
}


/*
 * LINUX specific code
 */


#else
typedef struct value_conf_type value_conf_type;
typedef struct section_conf_type section_conf_type;
struct value_conf_type
{
	char name[100];
	char value[100];
	value_conf_type *next;	/* Next value in list */
};
struct section_conf_type
{
	char name[100];
	value_conf_type *first;	/* First value in list */
	section_conf_type *next;	/* Next section in list */
};
static section_conf_type *root_node = NULL;
static bool conf_need_save = false;	/* Scheduled save */

/* Find a section by name */
section_conf_type* conf_get_section(const char* section)
{
	section_conf_type *s_ptr;
	for (s_ptr = root_node; s_ptr; s_ptr = s_ptr->next)
	{
		if ( !my_stricmp(section, s_ptr->name) )
		{
			return s_ptr;
		}
	}
	return NULL;
}
bool conf_section_exists(const char* section)
{
	if (conf_get_section(section) == NULL)
		return false;

	return true;
}
/* Add new section if it doesn't exist allready */
section_conf_type* conf_add_section_aux(const char* section)
{
	section_conf_type	*s_ptr;
	section_conf_type	*s_forge = NULL;

	/* Find section */
	s_ptr = conf_get_section(section);

	/* Not found */
	if (!s_ptr)
	{
		/* Forge new section */
		s_forge = 0;
		s_forge = malloc(sizeof(section_conf_type));

		/* Fill */
		my_strcpy(s_forge->name, section, 100);
		s_forge->next = NULL;
		s_forge->first = NULL;

		/* Attach */
		for (s_ptr = root_node; s_ptr->next; s_ptr = s_ptr->next) { }
		if (!s_ptr)
			root_node->next = s_forge;
		else
			s_ptr->next = s_forge;
		s_ptr = s_forge;

		conf_need_save = true;
	}

	return s_ptr;
}
void conf_add_section(const char* section)
{
	conf_add_section_aux(section);
}
/* Change a "string" prefrence and schedule save */
void conf_set_string(const char* section, const char* name, const char* value)
{
	section_conf_type	*s_ptr = NULL;
	value_conf_type 	*v_ptr;
	value_conf_type 	*v_forge = NULL;
	bool done = false;

	/* If section doesn't exist, create it */
	s_ptr = conf_get_section(section);
	if (!s_ptr)
		s_ptr = conf_add_section_aux(section);

	/* Find node to change */
	for (v_ptr = s_ptr->first; v_ptr; v_ptr = v_ptr->next)
	{
		if ( !my_stricmp(name, v_ptr->name) )
		{
			my_strcpy(v_ptr->value, value, 100);
			done = true;
			break;
		}
	}

	/* Or create new node */
	if (!done)
	{
		/* Forge */
		v_forge = 0;
		v_forge = malloc(sizeof(value_conf_type));

		/* Fill */
		strcpy(v_forge->name, name);
		strcpy(v_forge->value, value);
		v_forge->next = NULL;
		
		/* Attach */
		if (s_ptr->first)
			for (v_ptr = s_ptr->first; v_ptr->next; v_ptr = v_ptr->next) { }
		if (!v_ptr)
			s_ptr->first = v_forge;
		else
			v_ptr->next = v_forge;

		done = true;
	}

	if (done) conf_need_save = true;
}
/* Change an "integer" value. All values are stored as strings. */
void conf_set_int(const char* section, const char* name, s32b value)
{
	char s_value[100];
	sprintf(s_value, "%" PRId32, value);
	conf_set_string(section, name, s_value);
}
/*
 * Return value from section "section" , with name "name"
 * For string values, a "const char*" is returned, for integers "int".
 *
 * Not recommended for external usage, use "conf_get_int" and
 * "conf_get_string" instead.
 */
long conf_get_value(const char* section, const char* name, const char* default_value, bool is_int)
{
	section_conf_type	*s_ptr;
	value_conf_type 	*v_ptr;

	for (s_ptr = root_node; s_ptr; s_ptr = s_ptr->next)
	{
		if ( !my_stricmp(section, s_ptr->name) )
		{
			for (v_ptr = s_ptr->first; v_ptr; v_ptr = v_ptr->next)
			{
				if ( !my_stricmp(name, v_ptr->name) )
				{
					if (is_int)
						return atoi(v_ptr->value);
					return (long)v_ptr->value;
				}
			}
		}
	}
	if (is_int)
		return atoi(default_value);
	return (long)default_value;
}
s32b conf_get_int(const char* section, const char* name, s32b default_value)
{
	static char v_value[100];
	sprintf(v_value, "%" PRId32, default_value);
	return (u32b)conf_get_value(section, name, v_value, true);
}
const char* conf_get_string(const char* section, const char* name, const char* default_value)
{
	return (const char*)conf_get_value(section, name, default_value, false);
}
void conf_read_file(ang_file* config, section_conf_type *s_ptr, value_conf_type *v_ptr)
{
	section_conf_type	*s_forge = NULL;
	value_conf_type 	*v_forge = NULL;

	char buf[1024];
	char s_name[100], *name, *value;
	int n;

	/* File is opened (paranoia) */
	if (config)
	{
		/* Read line (till end of file) */
		while (file_getl(config, buf, 1024))
		{
			/* Skip comments, empty lines */
			if (buf[0] == '\n' || buf[0] == '#' || buf[0] == ';')
				continue;

			/* Probably a section */
			if (buf[0] == '[')
			{
				/* Trim */
				for(n = strlen(buf);
			 	((buf[n] == '\n' || buf[n] == '\r' || buf[n] == ' ' || !buf[n]) && n > 1);
			 	n--)	{ 	}

				/* Syntax is correct */
				if (buf[n] == ']' && n > 1)
				{
					/* Get name */
					buf[n] = '\0';
					strcpy(s_name, buf + 1);
					
					/* New section */
					if (!conf_section_exists(s_name)) 
					{
						/* Forge new section */
						s_forge = 0;
						s_forge = malloc(sizeof(section_conf_type));

						/* Fill */
						strcpy(s_forge->name, s_name);
						s_forge->next = NULL;
						s_forge->first = NULL;
						s_ptr->next = s_forge;
						s_ptr = s_forge;

						/* Attach */
						v_ptr = s_ptr->first;

						/* Done */
						continue;
					}
				}
				/* Malformed entry, skip */
				continue;
			}

			/* Attempt to read a value */
			name	= strtok(buf, " =\t\n");
			value	= strtok(NULL, " =\t\n");

			/* Read something */
			if (name && value)
			{
				/* Forge new node */
				v_forge = 0;
				v_forge = malloc(sizeof(value_conf_type));

				/* Fill */
				strcpy(v_forge->name, name);
				strcpy(v_forge->value, value);
				v_forge->next = NULL;

				/* Attach */
				if (!v_ptr)
					s_ptr->first = v_forge;
				else
					v_ptr->next = v_forge;

				/* Advance */
				v_ptr = v_forge;
			}
		}
	}
} 
/* Initialize global config tree */
void conf_init(void* param)
{
	section_conf_type	*s_ptr = NULL;
	value_conf_type 	*v_ptr = NULL;

	ang_file* config;
	char buf[1024];

	/*
	 * Prepare root node
	 */

	/* Forge root */
	if (!root_node)
		root_node = malloc(sizeof(section_conf_type));

	/* Prepare */
	strcpy(root_node->name, "MAngband");
	root_node->next = NULL;
	root_node->first = NULL;

	/* Attach */
	s_ptr = root_node;
	v_ptr = root_node->first;

	/* We start with closed file */
	config = NULL;

	/* Try to get path to config file from command-line "--config" option */
	if (clia_read_string(buf, 1024, "config"))
	{
		/* Attempt to open file */
		config = file_open(buf, MODE_READ, -1);
	}

	/*
	 * Get File name 
	 */

	/* EMX Hack */
#ifdef USE_EMX
	strcpy(buf, "\\mang.rc");
#else
	strcpy(buf, "/.mangrc");
#endif

	/* Hack -- make this file easier to find */
#if defined(__APPLE__) || defined(ON_XDG)
	strcpy(buf, "/mangclient.ini");
#endif

	/* Try to find home directory */
	if (!config && getenv("HOME"))
	{
		/* Use home directory as base */
		my_strcpy(config_name, getenv("HOME"), 1024);

		/* Append filename */
		my_strcat(config_name, buf, 1024);

		/* Attempt to open file */
		config = file_open(config_name, MODE_READ, -1);
	}

	/* Otherwise use current directory */
	if (!config)
	{
		/* Current directory */
		my_strcpy(config_name, ".", 1024);

		/* Append filename */
		my_strcat(config_name, buf, 1024);

		/* Attempt to open file */
		config = file_open(config_name, MODE_READ, -1);
	}

#if defined(ON_IOS) || (defined(ON_OSX) && !defined(HAVE_CONFIG_H))
	if (!config)
	{
		/* Application Support directory */
		appl_get_appsupport_dir(config_name, 1024, true);

		/* Append filename */
		my_strcat(config_name, buf, 1024);

		/* Attempt to open file */
		config = file_open(config_name, MODE_READ, -1);
    }
#endif

	/*
	 * Read data
	 */

	/* File is opened */
	if (config)
	{
		/* Use auxilary function */
		conf_read_file(config, s_ptr, v_ptr);

		/* Done reading */
		file_close(config);
	}
#if 0
	//list all sections
	for (s_ptr = root_node; s_ptr; s_ptr = s_ptr->next)
	{
		printf("[%s]\n", s_ptr->name);
		//list all values
		for (v_ptr = s_ptr->first; v_ptr; v_ptr = v_ptr->next)
		{
			printf("  %s = %s\n", v_ptr->name, v_ptr->value);
		}
	}
#endif
}
/* Destroy */
void conf_done()
{
	section_conf_type	*s_ptr = NULL;
	value_conf_type 	*v_ptr = NULL;

	section_conf_type	*s_ptr_next = NULL;
	value_conf_type 	*v_ptr_next = NULL;

	/* Delete all sections */
	for (s_ptr = root_node; s_ptr; s_ptr = s_ptr_next)
	{
		/* Delete all nodes */
		for (v_ptr = s_ptr->first; v_ptr; v_ptr = v_ptr_next)
		{
			v_ptr_next = v_ptr->next;
			free(v_ptr);
		}
		s_ptr_next = s_ptr->next;
		free(s_ptr);
	}
}
/* Save config file if it is scheduled */
void conf_save()
{
	section_conf_type *s_ptr;
	value_conf_type 	*v_ptr;
	ang_file* config;

	/* No changes */
	if (!conf_need_save) return;

	/* Write */
	if ((config = file_open(config_name, MODE_WRITE, FTYPE_TEXT)))
	{
		for (s_ptr = root_node; s_ptr; s_ptr = s_ptr->next)
		{
			file_putf(config, "[%s]\n", s_ptr->name);
			for (v_ptr = s_ptr->first; v_ptr; v_ptr = v_ptr->next)
			{
				file_putf(config, "%s %s\n", v_ptr->name, v_ptr->value);
			}
			if (s_ptr->next)
				file_putf(config, "\n");
		}
		/* Done writing */
		file_close(config);
		conf_need_save = false;
	}
}
/* Scheduler */
void conf_timer(int ticks)
{
	static int last_update = 0;
	if ((ticks - last_update) > 600) /* 60 seconds? */
	{
		conf_save();
		last_update = ticks;
	}
}
/* HACK: Append section from other file */
void conf_append_section(const char* sectionFrom, const char *sectionTo, const char* filename)
{
	ang_file* config;

	section_conf_type *s_ptr;
	value_conf_type 	*v_ptr;

	/* Find pointers */
	s_ptr = conf_add_section_aux(sectionTo);
	for (v_ptr = s_ptr->first; v_ptr; v_ptr = v_ptr->next) { }

	/* Try opening this 'other file' */
	config = file_open(filename, MODE_READ, -1);

	/* File is opened */
	if (config)
	{
		/* Use auxilary function */
		conf_read_file(config, s_ptr, v_ptr);

		/* Done reading */
		file_close(config);
	}
}
#endif


static int p_argc = 0;
static const char **p_argv = NULL;


void clia_init(int argc, const char **argv)
{
    /* If it's unsafe, we'll just copy */
    p_argc = argc;
    p_argv = argv;
}


static int clia_find(const char *key)
{
    int i;
    bool awaiting_argument = false;
    bool key_matched = false;
    bool got_hostname = false;

    for (i = 1; i < p_argc; i++)
    {
        if (prefix(p_argv[i], "--"))
        {
            const char *c = &p_argv[i][2];

            if (awaiting_argument && key_matched)
            {
                /*
                 * Hack -- if this is second --longopt in a row, and the
                 * last one was matching our key, assume we're done!
                 */
                return i - 1;
            }
            awaiting_argument = true;
            key_matched = false;
            if (!STRZERO(c) && streq(key, c)) key_matched = true;
        }
        else if (awaiting_argument)
        {
            awaiting_argument = false;
            if (key_matched)
            {
                /* Found */
                return i;
            }
        }
        else if (i == p_argc - 1 || (i == p_argc - 2 && !got_hostname))
        {
            /* Could be hostname */
            if (!got_hostname)
            {
                got_hostname = true;
                if (streq(key, "host"))
                {
                    /* Found */
                    return i;
                }
            }

            /* Port! */
            else
            {
                if (streq(key, "port"))
                {
                    /* Found */
                    return i;
                }
            }
        }
    }

    return -1;
}


static bool clia_cpy_string(char *dst, int len, int i)
{
    if (i > 0 && i < p_argc)
    {
        my_strcpy(dst, p_argv[i], len);
        return true;
    }
    return false;
}


static bool clia_cpy_int(s32b *dst, int i)
{
    if (i > 0 && i < p_argc)
    {
        *dst = atoi(p_argv[i]);
        return true;
    }
    return false;
}


bool clia_read_string(char *dst, int len, const char *key)
{
    int i = clia_find(key);

    return clia_cpy_string(dst, len, i);
}


bool clia_read_int(s32b *dst, const char *key)
{
    int i = clia_find(key);

    return clia_cpy_int(dst, i);
}
