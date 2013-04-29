#include "angbandcw.h"

#include <windows.h>

/**********************************************************\
*
*		Time Functions
*
\**********************************************************/
/***************************************************************************
Mimic the time function from time.h
***************************************************************************/
#if 0
time_t time_fromMessageBoard( time_t *timer )
{
	SYSTEMTIME sysTime, time_tStartTime;
	double dStartTime = 0, dSysTime = 0, dTempTime;
	long nSysTime = 0;
	
	/* Set the time to the start of the time_t data storage
	Midnight (00:00:00), January 1, 1970
	*/
	time_tStartTime.wDay = 1;
	time_tStartTime.wDayOfWeek = 4;
	time_tStartTime.wHour = 0;
	time_tStartTime.wMilliseconds = 0;
	time_tStartTime.wMinute = 0;
	time_tStartTime.wMonth = 1;
	time_tStartTime.wSecond = 0;
	time_tStartTime.wYear = 1970; 
	
	// Get current time
	GetSystemTime (&sysTime);
	
	/* Convert the SYSTEMTIME structures to variant time doubles
	A variant time is stored as an 8-byte real value (double), representing a 
	date between January 1, 100 and December 31, 9999, inclusive. The value 2.0 
	represents January 1, 1900; 3.0 represents January 2, 1900, and so on. 
	Adding 1 to the value increments the date by a day. The fractional part of 
	the value represents the time of day. Therefore, 2.5 represents noon on 
	January 1, 1900; 3.25 represents 6:00 A.M. on January 2, 1900, and so on. 
	Negative numbers represent dates prior to December 30, 1899. The variant 
	time resolves to one second. Any milliseconds in the input date are ignored. 
	*/
	SystemTimeToVariantTime (&time_tStartTime, &dStartTime);
	SystemTimeToVariantTime (&sysTime, &dSysTime);
	
	dTempTime = dSysTime - dStartTime;
	nSysTime = (int) (dTempTime * 86400);
	
	if (timer != NULL)
	{
		*timer = nSysTime;
	}
	return nSysTime;
}
#endif

unsigned long fake_time(unsigned long* fake_time_t)
{
	// I probably should be using SystemTimeToFileTime()
	// but it said it translated the TIkme to nanseconds from 
	// 1-1-1601 when I need seconds from 1970.
	unsigned long yearSeconds;
	unsigned long monthSeconds;
	unsigned long daySeconds;
	unsigned long hourSeconds;
	unsigned long minuteSeconds;
	unsigned long totalSeconds;

	SYSTEMTIME systemTime;
	int i;

	const int numSecondsYear = 365 * 24 * 60 * 60;

	/*
	const int numSecondsJan = 31 * 24 * 60 * 60;
	const int numSecondsFeb = 28 * 24 * 60 * 60;
	const int numSecondsMar = 31 * 24 * 60 * 60;
	const int numSecondsApr = 30 * 24 * 60 * 60;
	const int numSecondsMay = 31 * 24 * 60 * 60;
	const int numSecondsJun = 30 * 24 * 60 * 60;
	const int numSecondsJul = 31 * 24 * 60 * 60;
	const int numSecondsAug = 31 * 24 * 60 * 60;
	const int numSecondsSep = 30 * 24 * 60 * 60;
	const int numSecondsOct = 31 * 24 * 60 * 60;
	const int numSecondsNov = 30 * 24 * 60 * 60;
	*/

	const int numSecondsMonth[] = {31 * 24 * 60 * 60,
		28 * 24 * 60 * 60,
		31 * 24 * 60 * 60,
		30 * 24 * 60 * 60,
		31 * 24 * 60 * 60,
		30 * 24 * 60 * 60,
		31 * 24 * 60 * 60,
		31 * 24 * 60 * 60,
		30 * 24 * 60 * 60,
		31 * 24 * 60 * 60,
		30 * 24 * 60 * 60};
	
	GetSystemTime(&systemTime); 
	
	yearSeconds = systemTime.wYear - 1970;
	yearSeconds *= numSecondsYear;

	monthSeconds = 0;

	for (i=0; i< (systemTime.wMonth-1); i++)
	{
		monthSeconds += numSecondsMonth[i];
	}

	daySeconds = (systemTime.wDay - 1) * 24 * 60 * 60;

	hourSeconds = (systemTime.wHour - 1) * 60 * 60;

	minuteSeconds = (systemTime.wMinute - 1) * 60;

	totalSeconds = yearSeconds + monthSeconds + daySeconds + hourSeconds + minuteSeconds + (systemTime.wSecond - 1);

	if (fake_time_t)
	{
		*fake_time_t = totalSeconds;
	}

	return totalSeconds;
}


/* This seems to be used by ctime to return a string. I don't really 
see any other way doimg it without globals. */ 
char g_strTime[26];


char* fake_ctime(const unsigned long* fake_time_t)
{
	// How should this guy be returned? If I allocate some space
	// no one will delete right? But I can't return a local
	// variable right?
	// I probably should be using SystemTimeToFileTime()
	// but it said it translated the TIkme to nanseconds from 
	// 1-1-1601 when I need seconds from 1970.
	unsigned long year;
	unsigned long month;
	unsigned long day;
	unsigned long hour;
	unsigned long minute;
	unsigned long second;
	unsigned long dayOfWeek;
	
	unsigned long modifiedSeconds;
	int modifiedMonthSeconds;
	int i;
	char strMonth[4];
	char strDayOfWeek[4];
	//char strTime[26];
	
	const int numSecondsYear = 365 * 24 * 60 * 60;

	const int numSecondsMonth[] = {31 * 24 * 60 * 60,
		28 * 24 * 60 * 60,
		31 * 24 * 60 * 60,
		30 * 24 * 60 * 60,
		31 * 24 * 60 * 60,
		30 * 24 * 60 * 60,
		31 * 24 * 60 * 60,
		31 * 24 * 60 * 60,
		30 * 24 * 60 * 60,
		31 * 24 * 60 * 60,
		30 * 24 * 60 * 60};
	
	modifiedSeconds = *fake_time_t;
	
	year = *fake_time_t / numSecondsYear;
	modifiedSeconds = *fake_time_t % numSecondsYear;
	
	year += 1970;


	modifiedMonthSeconds = (int)modifiedSeconds;

	month = 0;

	for (i=0; i<12; i++)
	{
		modifiedMonthSeconds -= (int)numSecondsMonth[i];
		if (modifiedMonthSeconds < 0)
		{
			month = i;
			break;
		}
	}

	for (i=0; i<(int)month; i++)
	{
		modifiedSeconds -= numSecondsMonth[i];
	}

	day = modifiedSeconds / (24 * 60 * 60);
	modifiedSeconds %= (24 * 60 * 60);

	day++;

	hour = modifiedSeconds / (60 * 60);
	modifiedSeconds %= (60 * 60);

	minute = modifiedSeconds / (60);
	modifiedSeconds %= (60);

	second = modifiedSeconds;


	switch(month)
	{
	case 0:
		strcpy(strMonth, "Jan");
		break;
	case 1:
		strcpy(strMonth, "Feb");
		break;
	case 2:
		strcpy(strMonth, "Mar");
		break;
	case 3:
		strcpy(strMonth, "Apr");
		break;
	case 4:
		strcpy(strMonth, "May");
		break;
	case 5:
		strcpy(strMonth, "Jun");
		break;
	case 6:
		strcpy(strMonth, "Jul");
		break;
	case 7:
		strcpy(strMonth, "Aug");
		break;
	case 8:
		strcpy(strMonth, "Sep");
		break;
	case 9:
		strcpy(strMonth, "Oct");
		break;
	case 10:
		strcpy(strMonth, "Nov");
		break;
	case 11:
		strcpy(strMonth, "Dec");
		break;
	default:
		strcpy(strMonth, "???");
		break;
	}

	dayOfWeek = *fake_time_t;
	dayOfWeek /= (24 * 60 * 60);
	dayOfWeek -= 2;
	dayOfWeek %= 7;
	
	
	switch(dayOfWeek)
	{
	case 0:
		strcpy(strDayOfWeek, "Sun");
		break;
	case 1:
		strcpy(strDayOfWeek, "Mon");
		break;
	case 2:
		strcpy(strDayOfWeek, "Tue");
		break;
	case 3:
		strcpy(strDayOfWeek, "Wed");
		break;
	case 4:
		strcpy(strDayOfWeek, "Thu");
		break;
	case 5:
		strcpy(strDayOfWeek, "Fri");
		break;
	case 6:
		strcpy(strDayOfWeek, "Sat");
		break;
	}

	sprintf(g_strTime, "%s %s %2d %2d:%2d:%2d %d\n"
		, strDayOfWeek
		, strMonth
		, day
		, hour
		, minute
		, second
		, year);

	return g_strTime;
}

/**********************************************************\
*
*		Time Functions
*
\**********************************************************/




/**********************************************************\
*
*		INI Functions
*
\**********************************************************/

int ChopLeadingTrailingSpaces(char** ppStr)
{
	int i;
	int length;

	length = strlen(*ppStr);

	for (i=0; i<length; i++)
	{
		if ((*ppStr)[i] != ' ')
		{
			break;
		}

		(*ppStr)++;
	}

	length = strlen(*ppStr);

	for (i=length-1; i>=0; i--)
	{
		if ((*ppStr)[i] != ' ')
		{
			break;
		}

		(*ppStr)[i] = 0;
	}

	return 0;
}

int ReadLineSlow(HANDLE hFile, char* lpReturnedString, int length)
{
	DWORD nBytesToRead;
	DWORD nBytesRead;
	char ch;
	BOOL bResult;
	int offset;

	nBytesToRead = 1;
	offset = 0;

	do {
		bResult = ReadFile(hFile, &ch, nBytesToRead, &nBytesRead, NULL) ; 
		if (!bResult)
		{
			return -1;
		}

		// Check for end of file. 
		if ((bResult) &&  (nBytesRead == 0)) 
		{ 
			// we’re at the end of the file 
			return 1;
		}
		else
		{
			if (ch == '\n')
			{
				// end of line
				lpReturnedString[offset] = 0;
				offset++;
				
				return 0;
			}
			else
			{
				lpReturnedString[offset] = ch;
				offset++;
			}
		}

		/*
		if (ch == '\n')
		{
			// end of line
			lpReturnedString[offset] = 0;
			offset++;

			return 0;
		}
		*/

	} while (1);

	return -2;
}

BOOL FindSection(HANDLE hFile, const char* sectionName)
{
	char fileLine[1024];
	char* pLine;
	int rv;
	
	while (1)
	{
		rv = ReadLineSlow(hFile, fileLine, 1024);
		if ( (rv < 0) || (rv == 1) )
		{
			return FALSE;
		}
		else
		{
			pLine = strtok(fileLine, "]");

			if (pLine)
			{
				// move up one bracket
				pLine++;

				if (!strcmp(pLine, sectionName))
				{
					return TRUE;
				}
			}
		}
	}

	return FALSE;
}

BOOL GetSectionKeyString(HANDLE hFile, const char* keyName, char* keyValue, int length)
{
	char fileLine[1024];
	char* pLine;
	int rv;

	strcpy(keyValue, "");
	
	while (1)
	{
		rv = ReadLineSlow(hFile, fileLine, 1024);
		if ( (rv < 0) || (rv == 1) )
		{
			return FALSE;
		}
		else
		{
			pLine = strtok(fileLine, "=");

			if (pLine)
			{
				rv = ChopLeadingTrailingSpaces(&pLine);

				if (!strcmp(pLine, keyName))
				{
					pLine = strtok(NULL, "\n");

					if (pLine)
					{
						rv = ChopLeadingTrailingSpaces(&pLine);
						strcpy(keyValue, pLine);
					}

					return TRUE;
				}
			}
		}
	}

	return FALSE;
}


BOOL GetSectionKeyInt(HANDLE hFile, const char* keyName, int* pValue)
{
	char fileLine[1024];
	char* pLine;
	int rv;

	while (1)
	{
		rv = ReadLineSlow(hFile, fileLine, 1024);
		if ( (rv < 0) || (rv == 1) )
		{
			return FALSE;
		}
		else
		{
			pLine = strtok(fileLine, "=");

			if (pLine)
			{
				rv = ChopLeadingTrailingSpaces(&pLine);

				if (!strcmp(pLine, keyName))
				{
					pLine = strtok(NULL, "\n");

					if (pLine)
					{
						rv = ChopLeadingTrailingSpaces(&pLine);
						*pValue = atoi(pLine);
					}

					return TRUE;
				}
			}
		}
	}

	return FALSE;
}
/***
****/
/*
Return Values
The return value is the number of characters copied to the buffer, 
not including the terminating null character. 

If neither lpAppName nor lpKeyName is NULL and the supplied destination 
buffer is too small to hold the requested string, the string is truncated 
and followed by a null character, and the return value is equal to nSize minus one. 

If either lpAppName or lpKeyName is NULL and the supplied destination 
buffer is too small to hold all the strings, the last string is truncated 
and followed by two null characters. In this case, the return value is equal 
to nSize minus two. 


DWORD GetPrivateProfileString(
  LPCTSTR lpAppName,        // section name
  LPCTSTR lpKeyName,        // key name
  LPCTSTR lpDefault,        // default string
  LPTSTR lpReturnedString,  // destination buffer
  DWORD nSize,              // size of destination buffer
  LPCTSTR lpFileName        // initialization file name
);
*/

DWORD GetPrivateProfileString(
  char* lpAppName,        // section name
  char* lpKeyName,        // key name
  char* lpDefault,        // default string
  char* lpReturnedString,  // destination buffer
  DWORD nSize,              // size of destination buffer
  char* lpFileName        // initialization file name
)
{
	char keyValue[1024];
	TCHAR wcFileName[1024];
	HANDLE fileHandle;
	BOOL bResult;

	if (lpReturnedString == NULL)
	{
		return 0;
	}

	if (lpDefault)
	{
		strcpy(lpReturnedString, lpDefault);
	}
	else
	{
		lpReturnedString = NULL;
	}

	
	mbstowcs(wcFileName, lpFileName, 1024);
	
	fileHandle = CreateFile(wcFileName, 
		GENERIC_READ, 
		FILE_SHARE_READ, 
		NULL, 
		OPEN_EXISTING, 
		FILE_ATTRIBUTE_NORMAL, 
		0);
	
	if (fileHandle == INVALID_HANDLE_VALUE)
	{
		return 0;
	}
	
	bResult = FindSection(fileHandle, lpAppName);
	if (!bResult)
	{
		CloseHandle(fileHandle);
		return 0;
	}
	
	bResult = GetSectionKeyString(fileHandle, lpKeyName, keyValue, 1024);
	if (bResult)
	{
		CloseHandle(fileHandle);
		strcpy(lpReturnedString, keyValue);
		return strlen(lpReturnedString);
	}
	
	CloseHandle(fileHandle);
	
	return 0;
}


DWORD WritePrivateProfileString(
  char* lpAppName,        // section name
  char* lpKeyName,        // key name
  char* lpString,  // destination buffer
  char* lpFileName        // initialization file name
)
{
	//return 1;
	return 0;
}


/*
Return Values
The return value is the integer equivalent of the string 
following the specified key name in the specified initialization 
file. If the key is not found, the return value is the specified 
default value. If the value of the key is less than zero, the 
return value is zero. 


UINT GetPrivateProfileInt(
  LPCTSTR lpAppName,  // section name
  LPCTSTR lpKeyName,  // key name
  INT nDefault,       // return value if key name not found
  LPCTSTR lpFileName  // initialization file name
);

*/

unsigned int GetPrivateProfileInt(
  char* lpAppName,  // section name
  char* lpKeyName,  // key name
  int nDefault,       // return value if key name not found
  char* lpFileName  // initialization file name
)
{
	int returnValue;
	int keyValue;
	TCHAR wcFileName[1024];
	HANDLE fileHandle;
	BOOL bResult;

	returnValue = nDefault;
	
	mbstowcs(wcFileName, lpFileName, 1024);
	
	fileHandle = CreateFile(wcFileName, 
		GENERIC_READ, 
		FILE_SHARE_READ, 
		NULL, 
		OPEN_EXISTING, 
		FILE_ATTRIBUTE_NORMAL, 
		0);
	
	if (fileHandle != INVALID_HANDLE_VALUE)
	{
		bResult = FindSection(fileHandle, lpAppName);
		if (bResult)
		{
			bResult = GetSectionKeyInt(fileHandle, lpKeyName, &keyValue);
			if (bResult)
			{
				returnValue = keyValue;
			}
		}
		
		CloseHandle(fileHandle);
	}

	if (returnValue < 0)
	{
		returnValue = 0;
	}

	return (unsigned int)returnValue;
}

//----------------------------------------------------------------------------
// WritePrivateProfileInt:
//----------------------------------------------------------------------------
DWORD WritePrivateProfileInt(  char* lpAppName,  // pointer to section name
							 char* lpKeyName,  // pointer to key name
							 int nValue,   // pointer to int to add
							 char* lpFileName  // pointer to initialization filename
							 )
{
	return 0;
}



DWORD GetPrivateProfileString_Reg(
  char* lpAppName,        // section name
  char* lpKeyName,        // key name
  char* lpDefault,        // default string
  char* lpReturnedString,  // destination buffer
  DWORD nSize,              // size of destination buffer
  char* lpFileName        // initialization file name
)
{
	/* FWG 4-16-2001 */
	/* HACK If the str is angband.ini send it to the registry. */
	//char keyValue[1024];
	TCHAR wcTemp[1024];
	
	TCHAR wcKey[1024];
	HKEY hKey;
	DWORD dwFlag;
	char checkStr[1024];

	if (lpReturnedString == NULL)
	{
		return 0;
	}

	if (lpDefault)
	{
		strcpy(lpReturnedString, lpDefault);
	}
	else
	{
		lpReturnedString = NULL;
	}
	
	
	
	mbstowcs(wcTemp, lpFileName, 1024);
	
	_tcscpy(wcKey, _T("SOFTWARE\\FA"));
	//_tcscat(wcKey, _T("\\"));
	
	mbstowcs(wcTemp, lpAppName, 1024);
	_tcscat(wcKey, _T("\\"));
	_tcscat(wcKey, wcTemp);
	
	
	// Try to open the key
	if (RegOpenKeyEx(HKEY_LOCAL_MACHINE,wcKey,0,0,&hKey) == ERROR_SUCCESS)
	{	
		DWORD valueSize;
		
		mbstowcs(wcKey, lpKeyName, 1024);
		
		valueSize = 1024 * sizeof(TCHAR);
		
		if (RegQueryValueEx(
			hKey,
			wcKey,
			NULL,
			&dwFlag,
			(LPBYTE)wcTemp, 
			&valueSize) != ERROR_SUCCESS)
		{
			RegCloseKey(hKey);
			return 0;
		}
		
		RegCloseKey(hKey);
		
		wcstombs(lpReturnedString, wcTemp, 1024);
		return strlen(lpReturnedString);
	}
	
	return 0;
}


DWORD WritePrivateProfileString_Reg(
  char* lpAppName,        // section name
  char* lpKeyName,        // key name
  char* lpString,  // destination buffer
  char* lpFileName        // initialization file name
)
{
	/* FWG 4-16-2001 */
	/* HACK If the str is angband.ini send it to the registry. */
	//char keyValue[1024];
	TCHAR wcTemp[1024];
	
	TCHAR wcKey[1024];
	HKEY hKey;
	DWORD dwDisposition;

	char checkStr[1024];

	if (lpString == NULL)
	{
		return 0;
	}

	_tcscpy(wcKey, _T("SOFTWARE\\FA"));
	//_tcscat(wcKey, _T("\\"));
	
	mbstowcs(wcTemp, lpAppName, 1024);
	_tcscat(wcKey, _T("\\"));
	_tcscat(wcKey, wcTemp);


	if (RegCreateKeyEx( 
		HKEY_LOCAL_MACHINE, 
		wcKey, 
		0, 
		_T("TOME"), 
		0, 
		0, 
		NULL, 
		&hKey, 
		&dwDisposition ) == ERROR_SUCCESS) 
	// Try to open the key
	//if (RegOpenKeyEx(HKEY_LOCAL_MACHINE,wcKey,0,0,&hKey) == ERROR_SUCCESS)
	{	
		int newKey = 0;

		if (dwDisposition == REG_CREATED_NEW_KEY)
		{
			newKey = 1;
		}

		mbstowcs(wcKey, lpKeyName, 1024);
		mbstowcs(wcTemp, lpString, 1024);
		
		if (RegSetValueEx(
			hKey,
			wcKey,
			(void*)0,
			REG_SZ,
			(LPBYTE)wcTemp, 
			(lstrlen(wcTemp)+1)*sizeof(TCHAR)) != ERROR_SUCCESS)
		{
			RegCloseKey(hKey);
			return 0;
		}

		RegCloseKey(hKey);
	}

	return 1;
}


/*
Return Values
The return value is the integer equivalent of the string 
following the specified key name in the specified initialization 
file. If the key is not found, the return value is the specified 
default value. If the value of the key is less than zero, the 
return value is zero. 


UINT GetPrivateProfileInt(
  LPCTSTR lpAppName,  // section name
  LPCTSTR lpKeyName,  // key name
  INT nDefault,       // return value if key name not found
  LPCTSTR lpFileName  // initialization file name
);

*/

unsigned int GetPrivateProfileInt_Reg(
  char* lpAppName,  // section name
  char* lpKeyName,  // key name
  int nDefault,       // return value if key name not found
  char* lpFileName  // initialization file name
)
{
	/* FWG 4-16-2001 */
	/* HACK If the str is angband.ini send it to the registry. */
	//char keyValue[1024];
	TCHAR wcTemp[1024];
	
	TCHAR wcKey[1024];
	HKEY hKey;
	DWORD dwFlag;

	int returnValue;
	int keyValue;
	TCHAR wcFileName[1024];
	HANDLE fileHandle;
	BOOL bResult;

	char checkStr[1024];

	returnValue = nDefault;
	
	
	
	
	_tcscpy(wcKey, _T("SOFTWARE\\FA"));
	//_tcscat(wcKey, _T("\\"));
	
	mbstowcs(wcTemp, lpAppName, 1024);
	_tcscat(wcKey, _T("\\"));
	_tcscat(wcKey, wcTemp);
	
	
	// Try to open the key
	if (RegOpenKeyEx(HKEY_LOCAL_MACHINE,wcKey,0,0,&hKey) == ERROR_SUCCESS)
	{
		DWORD valueSize;
		
		mbstowcs(wcKey, lpKeyName, 1024);
		
		valueSize = 1024 * sizeof(TCHAR);
		
		if (RegQueryValueEx(
			hKey,
			wcKey,
			NULL,
			&dwFlag,
			(LPBYTE)&wcTemp, 
			&valueSize) != ERROR_SUCCESS)
		{
			RegCloseKey(hKey);
			return returnValue;
		}
		
		RegCloseKey(hKey);
		
		returnValue = _ttoi(wcTemp);
		
		/*
		if (returnValue < 0)
		{
		returnValue = 0;
		}
		
		  return returnValue;
		*/
	}

	if (returnValue < 0)
	{
		returnValue = 0;
	}

	return (unsigned int)returnValue;
}

//----------------------------------------------------------------------------
// WritePrivateProfileInt:
//----------------------------------------------------------------------------
DWORD WritePrivateProfileInt_Reg(  char* lpAppName,  // pointer to section name
							 char* lpKeyName,  // pointer to key name
							 int nValue,   // pointer to int to add
							 char* lpFileName  // pointer to initialization filename
							 )
{
	/* FWG 4-16-2001 */
	/* HACK If the str is angband.ini send it to the registry. */
	//char keyValue[1024];
	TCHAR wcTemp[1024];
	
	TCHAR wcKey[1024];
	HKEY hKey;
	
	char checkStr[1024];

	_tcscpy(wcKey, _T("SOFTWARE\\FA"));
	//_tcscat(wcKey, _T("\\"));
	
	mbstowcs(wcTemp, lpAppName, 1024);
	_tcscat(wcKey, _T("\\"));
	_tcscat(wcKey, wcTemp);


	// Try to open the key
	if (RegOpenKeyEx(HKEY_LOCAL_MACHINE,wcKey,0,0,&hKey) == ERROR_SUCCESS)
	{	
		DWORD inReg;

		mbstowcs(wcKey, lpKeyName, 1024);
		//mbstowcs(wcTemp, lpString, 1024);
		
		inReg = nValue;

		if (RegSetValueEx(
			hKey, 
			wcKey,
			(void*)0,
			REG_DWORD,
			(LPBYTE)(&inReg), 
			sizeof(DWORD)) != ERROR_SUCCESS)
		{
			RegCloseKey(hKey);
			return 0;
		}

		RegCloseKey(hKey);
	}

	return 1;
}




/**********************************************************\
*
*		INI Functions
*
\**********************************************************/

