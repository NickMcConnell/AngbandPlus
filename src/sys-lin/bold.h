// Font: bold

int font_bold_height = 16;
int font_bold_overhang = 0;

int font_bold_widths[96] = {
    4,
    4,
    8,
    7,
    7,
    15,
    12,
    4,
    5,
    5,
    7,
    8,
    4,
    5,
    4,
    4,
    7,
    6,
    7,
    7,
    7,
    7,
    7,
    7,
    7,
    7,
    5,
    5,
    8,
    8,
    8,
    7,
    13,
    10,
    9,
    10,
    10,
    8,
    8,
    10,
    10,
    6,
    7,
    10,
    9,
    12,
    9,
    11,
    8,
    11,
    10,
    8,
    8,
    9,
    10,
    14,
    10,
    10,
    8,
    5,
    4,
    5,
    8,
    7,
    5,
    7,
    7,
    6,
    7,
    7,
    5,
    7,
    8,
    4,
    4,
    9,
    4,
    12,
    8,
    7,
    7,
    7,
    6,
    6,
    5,
    8,
    7,
    9,
    7,
    7,
    6,
    6,
    3,
    6,
    7,
    11,
};

u32b font_bold_data[96*16] = {
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000004, 0x00000004, 0x00000004, 0x00000000, 0x00000006, 0x00000006, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000066, 0x00000066, 0x00000066, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000024, 0x00000024, 0x00000022, 0x0000003F, 0x00000012, 0x00000012, 0x0000003F, 0x00000009, 0x00000009, 0x00000009, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000008, 0x0000001C, 0x0000002A, 0x0000002A, 0x0000000E, 0x0000000C, 0x00000018, 0x00000038, 0x0000002A, 0x0000002A, 0x0000001C, 0x00000008, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000041C, 0x00000236, 0x00000136, 0x00000136, 0x00001CB6, 0x0000369C, 0x00003640, 0x00003620, 0x00003620, 0x00001C10, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x000000E0, 0x00000130, 0x00000130, 0x000000B0, 0x00000770, 0x0000026C, 0x00000166, 0x000000C6, 0x000005CE, 0x000003BC, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000006, 0x00000006, 0x00000006, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000010, 0x00000008, 0x0000000C, 0x00000004, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000004, 0x0000000C, 0x00000008, 0x00000010, 0x00000000, 
    0x00000000, 0x00000000, 0x00000001, 0x00000002, 0x00000006, 0x00000004, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x00000004, 0x00000006, 0x00000002, 0x00000001, 0x00000000, 
    0x00000000, 0x00000000, 0x00000008, 0x00000008, 0x0000003E, 0x00000008, 0x00000014, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000008, 0x00000008, 0x00000008, 0x0000007F, 0x00000008, 0x00000008, 0x00000008, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000006, 0x00000006, 0x00000004, 0x00000002, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0000000F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000006, 0x00000006, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000008, 0x00000008, 0x00000004, 0x00000004, 0x00000004, 0x00000002, 0x00000002, 0x00000002, 0x00000001, 0x00000001, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000001E, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x0000001E, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000000C, 0x0000000E, 0x0000000F, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000003F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000001C, 0x0000003E, 0x00000033, 0x00000030, 0x00000010, 0x00000010, 0x00000008, 0x00000004, 0x0000003E, 0x0000003F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000001C, 0x0000003E, 0x00000031, 0x00000010, 0x0000000C, 0x00000038, 0x00000030, 0x00000030, 0x00000013, 0x0000000F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000010, 0x00000018, 0x00000018, 0x0000001C, 0x0000001A, 0x0000001A, 0x00000019, 0x0000003F, 0x00000018, 0x00000018, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000003C, 0x0000001E, 0x00000002, 0x0000000E, 0x0000001F, 0x00000038, 0x00000020, 0x00000020, 0x00000013, 0x0000000F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000038, 0x0000000C, 0x00000006, 0x0000001F, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x0000001E, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000003E, 0x0000003E, 0x00000020, 0x00000010, 0x00000010, 0x00000010, 0x00000008, 0x00000008, 0x00000008, 0x00000008, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000001E, 0x00000033, 0x00000033, 0x00000017, 0x0000000E, 0x0000001C, 0x0000003A, 0x00000033, 0x00000033, 0x0000001E, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000001E, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x0000001E, 0x00000018, 0x0000000C, 0x00000003, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000006, 0x00000006, 0x00000000, 0x00000000, 0x00000000, 0x00000006, 0x00000006, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000006, 0x00000006, 0x00000000, 0x00000000, 0x00000000, 0x00000006, 0x00000006, 0x00000004, 0x00000002, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000040, 0x00000038, 0x00000006, 0x00000001, 0x00000006, 0x00000038, 0x00000040, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0000007F, 0x00000000, 0x0000007F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000001, 0x0000000E, 0x00000030, 0x00000040, 0x00000030, 0x0000000E, 0x00000001, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000001C, 0x00000036, 0x00000036, 0x00000030, 0x00000010, 0x00000008, 0x00000008, 0x00000000, 0x0000000C, 0x0000000C, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x000001E0, 0x00000210, 0x00000408, 0x00000AC4, 0x00000B62, 0x00000B32, 0x00000932, 0x00000532, 0x000003B2, 0x00000964, 0x00000408, 0x00000210, 0x000001E0, 0x00000000, 
    0x00000000, 0x00000000, 0x00000020, 0x00000030, 0x00000070, 0x00000068, 0x00000068, 0x000000C8, 0x000000FC, 0x000000C4, 0x00000182, 0x000003CF, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000007F, 0x000000C6, 0x000000C6, 0x00000066, 0x0000003E, 0x00000066, 0x000000C6, 0x000000C6, 0x000000C6, 0x0000007F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000178, 0x0000018C, 0x00000106, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x0000010C, 0x000000F8, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000007F, 0x000000C6, 0x00000186, 0x00000186, 0x00000186, 0x00000186, 0x00000186, 0x00000186, 0x000000C6, 0x0000007F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x000000FF, 0x00000086, 0x00000006, 0x00000026, 0x0000003E, 0x00000026, 0x00000006, 0x00000006, 0x00000086, 0x000000FF, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x000000FF, 0x00000086, 0x00000006, 0x00000026, 0x0000003E, 0x00000026, 0x00000006, 0x00000006, 0x00000006, 0x0000000F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000178, 0x0000018C, 0x00000106, 0x00000006, 0x00000006, 0x00000006, 0x000003C6, 0x00000186, 0x0000018C, 0x000000F8, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x000003CF, 0x00000186, 0x00000186, 0x00000186, 0x000001FE, 0x00000186, 0x00000186, 0x00000186, 0x00000186, 0x000003CF, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000001E, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000001E, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000078, 0x00000030, 0x00000030, 0x00000030, 0x00000030, 0x00000030, 0x00000030, 0x00000033, 0x00000033, 0x0000001E, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x000003CF, 0x00000086, 0x00000046, 0x00000026, 0x0000001E, 0x00000036, 0x00000036, 0x00000066, 0x000000C6, 0x000003EF, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000000F, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000086, 0x00000086, 0x000000C6, 0x000000FF, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000F0F, 0x0000070E, 0x0000070E, 0x0000069A, 0x0000069A, 0x0000069A, 0x00000672, 0x00000672, 0x00000662, 0x00000F27, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x000001CF, 0x0000008E, 0x0000009A, 0x0000009A, 0x000000B2, 0x000000E2, 0x000000E2, 0x000000C2, 0x00000082, 0x00000087, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x000000F8, 0x0000018C, 0x00000306, 0x00000306, 0x00000306, 0x00000306, 0x00000306, 0x00000306, 0x0000018C, 0x000000F8, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000007F, 0x000000C6, 0x000000C6, 0x000000C6, 0x000000C6, 0x0000007E, 0x00000006, 0x00000006, 0x00000006, 0x0000000F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x000000F8, 0x0000018C, 0x00000306, 0x00000306, 0x00000306, 0x00000306, 0x00000306, 0x00000306, 0x0000018C, 0x000000F8, 0x00000070, 0x00000060, 0x000001C0, 0x00000000, 
    0x00000000, 0x00000000, 0x0000007F, 0x000000C6, 0x000000C6, 0x000000C6, 0x0000007E, 0x00000036, 0x00000066, 0x00000066, 0x000000C6, 0x000003CF, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000005C, 0x00000066, 0x00000046, 0x0000000E, 0x0000001C, 0x00000038, 0x00000070, 0x00000062, 0x00000066, 0x0000003A, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x000000FF, 0x00000099, 0x00000018, 0x00000018, 0x00000018, 0x00000018, 0x00000018, 0x00000018, 0x00000018, 0x0000003C, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x000001CF, 0x00000086, 0x00000086, 0x00000086, 0x00000086, 0x00000086, 0x00000086, 0x00000086, 0x00000086, 0x0000007C, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000038F, 0x00000106, 0x0000010C, 0x0000008C, 0x0000008C, 0x00000058, 0x00000058, 0x00000030, 0x00000030, 0x00000030, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00003BEF, 0x00001186, 0x0000118C, 0x0000118C, 0x00000B58, 0x00000B58, 0x00000B58, 0x00000630, 0x00000630, 0x00000630, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x000003CF, 0x00000086, 0x0000004C, 0x00000058, 0x00000038, 0x00000030, 0x00000068, 0x00000068, 0x000000C6, 0x000003EF, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000038F, 0x00000106, 0x0000008C, 0x0000008C, 0x00000058, 0x00000038, 0x00000030, 0x00000030, 0x00000030, 0x00000078, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x000000FE, 0x00000066, 0x00000072, 0x00000030, 0x00000018, 0x00000018, 0x0000008C, 0x0000008E, 0x000000C6, 0x000000FF, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000000E, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x0000000E, 0x00000000, 
    0x00000000, 0x00000000, 0x00000001, 0x00000001, 0x00000002, 0x00000002, 0x00000002, 0x00000004, 0x00000004, 0x00000004, 0x00000008, 0x00000008, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000000E, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000E, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000008, 0x00000014, 0x00000022, 0x00000022, 0x00000041, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0000007F, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000002, 0x00000004, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0000001E, 0x00000033, 0x00000038, 0x00000036, 0x00000033, 0x00000033, 0x0000007E, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000003, 0x00000003, 0x00000003, 0x0000001F, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x0000001D, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0000001E, 0x0000001B, 0x00000003, 0x00000003, 0x00000003, 0x00000013, 0x0000000E, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000038, 0x00000030, 0x00000030, 0x0000003E, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x0000007E, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0000001E, 0x00000033, 0x00000033, 0x0000003F, 0x00000003, 0x00000023, 0x0000001E, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x0000001C, 0x00000016, 0x00000006, 0x0000000F, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x0000000F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0000007E, 0x00000033, 0x00000033, 0x00000033, 0x0000001E, 0x00000001, 0x0000003F, 0x0000007F, 0x00000041, 0x0000003E, 0x00000000, 
    0x00000000, 0x00000000, 0x00000007, 0x00000006, 0x00000006, 0x00000036, 0x0000006E, 0x00000066, 0x00000066, 0x00000066, 0x00000066, 0x000000EF, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000006, 0x00000006, 0x00000000, 0x00000007, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x0000000F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000006, 0x00000006, 0x00000000, 0x00000007, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000003, 0x00000000, 
    0x00000000, 0x00000000, 0x00000007, 0x00000006, 0x00000006, 0x000000E6, 0x00000026, 0x00000016, 0x0000001E, 0x00000036, 0x00000066, 0x000000EF, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000007, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x0000000F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x000003B7, 0x0000066E, 0x00000666, 0x00000666, 0x00000666, 0x00000666, 0x00000EEF, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000037, 0x0000006E, 0x00000066, 0x00000066, 0x00000066, 0x00000066, 0x000000EF, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0000001E, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x0000001E, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0000001F, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x0000001F, 0x00000003, 0x00000003, 0x00000007, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0000002E, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x00000033, 0x0000003E, 0x00000030, 0x00000030, 0x00000078, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000037, 0x0000002E, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x0000000F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0000001E, 0x00000013, 0x00000007, 0x0000000E, 0x0000001C, 0x00000019, 0x0000000F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000004, 0x00000004, 0x0000000F, 0x00000006, 0x00000006, 0x00000006, 0x00000006, 0x00000016, 0x0000000C, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000077, 0x00000066, 0x00000066, 0x00000066, 0x00000066, 0x00000066, 0x000000FC, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000077, 0x00000023, 0x00000016, 0x00000016, 0x0000000E, 0x0000000C, 0x0000000C, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x000001BB, 0x00000093, 0x000000BB, 0x000000BB, 0x00000066, 0x00000066, 0x00000066, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0000006F, 0x00000026, 0x0000001C, 0x0000000C, 0x0000001C, 0x00000032, 0x0000007B, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000077, 0x00000026, 0x00000026, 0x0000001C, 0x0000001C, 0x00000018, 0x00000008, 0x00000008, 0x0000000B, 0x00000007, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x0000001F, 0x00000019, 0x0000000C, 0x0000000C, 0x00000006, 0x00000016, 0x0000001F, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x00000018, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x00000004, 0x00000002, 0x00000004, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x00000018, 0x00000000, 
    0x00000000, 0x00000000, 0x00000002, 0x00000002, 0x00000002, 0x00000002, 0x00000002, 0x00000002, 0x00000002, 0x00000002, 0x00000002, 0x00000002, 0x00000002, 0x00000002, 0x00000002, 0x00000000, 
    0x00000000, 0x00000000, 0x00000006, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x00000008, 0x00000010, 0x00000008, 0x0000000C, 0x0000000C, 0x0000000C, 0x0000000C, 0x00000006, 0x00000000, 
    0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000027, 0x00000039, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
    0x00000000, 0x00000000, 0x000001FC, 0x00000104, 0x00000104, 0x00000104, 0x00000104, 0x00000104, 0x00000104, 0x00000104, 0x00000104, 0x000001FC, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 
};

