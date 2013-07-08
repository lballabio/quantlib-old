/*
*+----------------------------------------------------------------------
*| File.........: DATECLS.CPP
*| Date.........: Sat  09-25-1993
*| Author.......: James M. Curran, et al
*| Version......: 5.0   Compile w/MSVC++ 1.0  / or Borland C++ 3.1
*| Usage........: General purpose date conversion, arithmetic,
*|              :    comparison, and formatting class
*|
*| See DATE.H for acknowledgements and compile/link notes.
*+----------------------------------------------------------------------
*/
#include "datecl.h"

#include <assert.h>
#include <ctype.h>

//--------------------- Class Varaibles ------------------------------------------------

int				Date::DisplayFormat		= Date::MDY;
unsigned int	Date::DisplayOptions	= 0;
unsigned short	Date::DefaultCentury    = 1900;
unsigned int   	Date::startDST=((Date::APRIL<<8) + 1);
unsigned int   	Date::startSTD=((Date::OCTOBER<<8) + 31);


//---------------------- Compatibility Section -----------------------------------------
//
// Here we attempt to smooth out all the variations between different compiliers, by 
// #define-ing several symbols to include or remove, or to use a common name.
//
// The #defines used are :
//

#if defined(_MSC_VER) || defined(_WIN32)

		#define	strnset		_strnset	// MS uses ANSI-friendly names for the
		#define	stricmp		_stricmp	// common (but non-standard) string 
		#define	strnicmp	_strnicmp	// functions

#elif defined (__ZTC__)  && __ZTC__ < 0x0600

		#define	stricmp(a,b)	memicmp(a,b, strlen(a))
		#define	strnicmp		memicmp

		// A quick check indicated that Zortech doesn't have a
		// stricmp function--- This may have to be changed. - JMC

		// This has been tested on the striped-down version of
		// ZTC that was provided with Tom Swan's 1991 book "Learning C++"
		
		// Of course, Zortech is now owned by Symantec, so they might
		// have their own predefined macro to identify themselves.
		// If anyone knows, please alert me... - JMC
#endif			
	
/*
#if !defined(_INC_WINDOWS)	// if this is NOT a windows program

                  
// See last of these blocks for English names & comments
                   
#if defined(_DEUTSCHEN)	//--------------------------------------------------------------------

		static const char *dayname[] = {"Sonntag","Montag","Dienstag",
					"Mittwoch","Donnerstag","Freitag","Samstag"} ;

		static const char *mname[] = {"Januar","Februar","Marz","April","Mai",
				   	"Juni","Juli","August","September","Oktober","November",
					"Dezember"};

		static const char	szInvalidDay[] 		= "Invalide Tag";
		static const char	szInvalidDate[] 	= "Invalide Datum";
		static const char	szInvalidMonth[]	= "Invalide Monat";
		static const char	szBCE[]				= " B.C.E.";
		static const char	szYears[]			= "Jahre";
		static const char	szMonths[]			= "Monate";
		static const char	szDays[]			= "Daten";

#elif defined(_ESPANOL)  //------------------------------------------------------------------


		static const *char dayname[]= {"Domingo","Lunes","Martes",
					"Miercoles","Jueves","Viernes","Sabado"} ;

		static const char *mname[] = {"Enero","Febrero","Marzo","Abril",
					"Mayo","Junio","Julio","Agosto","Septiembre","Octubre",
					"Noviembre","Diciembre"};

		static const char	szInvalidDay[] 		= "Invalido dia";
		static const char	szInvalidDate[] 	= "Invalido data";
		static const char	szInvalidMonth[]	= "Invalido mes";
		static const char	szBCE[]				= " B.C.E.";
		static const char	szYears[]			= "anos";
		static const char	szMonths[]			= "mes";
		static const char	szDays[]			= "dia";

#elif defined (_FRANCAIS)
		static const char *dayname[]= {"Dimanche","Lundi","Mardi",
					"Mercredi", "Jeudi","Vendredi","Samedi"} ;

		static const char *mname[] = {"Janvier","Fevrier","Mars","Avril",
					"Mai","Juin","Juillet","Aout","Septembre","Octobre",
					"Novembre","Decembre"};

		static const char	szInvalidDay[] 		= "invalide jour";
		static const char	szInvalidDate[] 	= "invalide date";
		static const char	szInvalidMonth[]	= "invalide mois";
		static const char	szBCE[]				= " B.C.E.";
		static const char	szYears[]			= "annee";
		static const char	szMonths[]			= "mois";
		static const char	szDays[]			= "journee";

*/
//#else   // ENGLISH

						// ---------------------------------------------------------------------

		
		static const char *dayname[]= {"Sunday","Monday","Tuesday",
					"Wednesday", "Thursday","Friday","Saturday"} ;

		static const char *mname[] = {"January","February","March","April",
					"May","June","July","August","September","October",
					"November","December"};

		static const char	szInvalidDay[] 		= "invalid day";
		static const char	szInvalidDate[] 	= "invalid date";
		static const char	szInvalidMonth[]	= "invalid month";
		static const char	szBCE[]				= " B.C.E.";
		static const char	szYears[]			= "years";
		static const char	szMonths[]			= "months";
		static const char	szDays[]			= "days";

//#endif
							// -----------------------------------------------------------------

	#define	GetString(x)			(x)
	#define GetStringArray(x,y)		(x##[y-1])

	#define	INVALID_DAY			szInvalidDay
	#define	INVALID_DATE		szInvalidDate
	#define	INVALID_MONTH		szInvalidMonth
	#define	MONTHS				mname
	#define	DAYNAME				dayname

/*
 * OK, the deal here is to use fake access functions to the strings
 * we've just defined above, but only if this is NOT a Windows program.
 * "Why?" you may be asking... Glad you asked.  If this were a Windows
 * EXE, or better yet, a DLL, we'd want to put those strings in a
 * STRINGTABLE, which means we'll need an access function to get them.
 * And unless we want to have a DOS version and a separate Windows 
 * version, we'll need access functions for the DOS version too.
 * But it's ineffecient to have REAL access funcs, when we can just
 * fake it using a couple of #defines.  Here a "call" to 
 * "GetString(INVALID_DATE)" would be exactly the same as using 
 * "szInvalidDate" in the code.  Similarly, "GetStringArray(MONTHS,month)"
 * is the same as "mname[month-1];".
 * BUT -- When  it comes time to write the Windows version, all we
 * need do is write simple functions for GetString & GetStringArray, 
 * and replace the #defines above with numeric constants. Viola!
 *
 * In fact here's a start for the Windows functions:
 *		#define	GetStringArray(x,y)	GetString(x+y)
 * -JMC
 */


//#endif   // (_INC_WINDOWS)


static char GauDays[]	= { 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

static int	DaysSoFar[][13] =
			{
			{0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365},
			{0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366}
			};



////////////////////////////////////////////////////////////
// Constructors
////////////////////////////////////////////////////////////

Date::Date()
{
	month 	= NON_MONTH;
	day 	= 0;
	year 	= 0;
	julian 	= 0L;

	day_of_week = NON_DAY;
}

//////////////////////////////////////////////////////////////

Date::Date (long j) : julian(j)
{
	julian_to_mdy ();
}

//////////////////////////////////////////////////////////////

Date::Date (short m, short d, short y) : month(m), day(d), year(y)
{
	assert(month > 0);
	assert(month < 13);

	assert(day   > 0);
	assert(day   < 32);

	mdy_to_julian ();
}

//////////////////////////////////////////////////////////////

Date::Date (const tm	&TM)
{
		month	= (short) (TM.tm_mon + 1);
		day		= (short)  TM.tm_mday;
		year	= (short) (TM.tm_year + 1900);

		mdy_to_julian();
}


//////////////////////////////////////////////////////////////

#if defined(MSDOS) || defined(_WIN32)
Date::Date (const DOSDATE_T &ds)
{
		month	= ds.month;
		day		= ds.day;
		year	= (short) ds.year;

		mdy_to_julian ();
}
#endif


//////////////////////////////////////////////////////////////

Date::Date (const Date &dt)
{
		month = dt.month;
		day   = dt.day;
		year  = dt.year;

		mdy_to_julian ();
}


//////////////////////////////////////////////////////////////

Date::Date (int weeknum, int dow, short m, short y) : month(m), day(1), year(y)
{
		int	d;

		assert(weeknum > -1);		// weeknum = 0 mean "the last" 
		assert(weeknum < 6);
		assert(dow >  NON_DAY);
		assert(dow <= SATURDAY);
		assert(m   >  NON_MONTH);
		assert(m   <= DECEMBER);
		
		mdy_to_julian ();

        if (weeknum ==0)
		{
				AddMonths(1);
		}

		d = (dow - day_of_week);
		julian = julian + d;

		if (d > -1) 
			weeknum--;
			
		AddWeeks(weeknum);
		
}
//////////////////////////////////////////////////////////////

Date::Date (const char *dat)
{
	tm			*local_time;
	time_t		 timer;
	const char	*pDat;

	timer	   = time(NULL);
	local_time = localtime(&timer);

	if ( (*dat == '.') || (stricmp(dat, "TODAY") == 0) )
	{
		month = (short)(local_time->tm_mon + 1);
		day   = (short) local_time->tm_mday;
		year  = (short)(local_time->tm_year + 1900);
	}
	else
	{
			int  	nlen=strlen(dat);
			int		i;
			int		datetype;
			int		mon;

        //
        //  Possible date string formats are those that are generated by the
        //  Date class itself!  The corresponding possible string lengths are
        //  also listed (with ranges from shortest to longest string for that
        //  format).
        //
		//	MDY:		03/23/1993				=> 6-10,  13-17
		//	COLLATE:	19930323				=> 8
		//	EUROPEAN:	23 March 1993			=> 13,	  20
		//	FULL,ABBR.: Tue, Mar 23, 1993		=> 16-17, 23-24
		//	FULL:		Tuesday, March 23, 1993	=> 22-23, 29-30
        //
        //  These dates may also have B.C.E. appended at the end, thus we have
        //  the second set of string lengths with 7 more characters!
        //

		if (isalpha(dat[0]))
				datetype = Date::FULL;

		else if (nlen == 20)
				datetype = Date::EUROPEAN;

		else if (nlen == 13)
				datetype = (isalpha(dat[5]) ? Date::EUROPEAN : Date::MDY);

		else
				datetype = Date::MDY;		// or COLLATE - both done at default:

		switch(datetype)
		{
			case Date::EUROPEAN:
				day   = (short) atoi(dat);

				pDat  = strchr(dat, ' ')+1;
				i 		= 12;

				while (i > 0)
				{
					if (strnicmp(pDat, GetStringArray(MONTHS,i), 3) == 0)
							break;

					i--;
				}

				mon   =  i;
				pDat  = strchr(pDat, ' ')+1;
				year  = (short) atoi(pDat);
				break;

			case Date::FULL:
				pDat = strchr(dat, ' ')+1;		// Skip the day info

				i 		= 12;

				while (i > 0)
				{
						if (strnicmp(pDat, GetStringArray(MONTHS,i), 3) == 0)
								break;

						i--;
				}

				mon	  = i;

				pDat  = strchr(pDat, ' ')+1;
				day   = (short) atoi(pDat);

				pDat  = strchr(pDat, ' ')+1;
				year  = (short) atoi(pDat);
				break;


			default:
				i = strspn(dat,"0123456789");

				if (i == nlen)		// It's entirely number --> COLLATE
				{
						year  = (short) ((dat[0]-'0')*1000 + (dat[1]-'0') *100  +
								         (dat[2]-'0')*10   + (dat[3]-'0'));
						// The above line sometimes produces a warning because the compiler
						// assumes that "(dat[0]-'0')*1000" can go beyond the range of a
						// short.  But the lines above it make sure it won't get higher
                        // than 9000, so every thing cool.

						mon   = (short) (dat[4]-'0')*10   + (dat[5]-'0');
						day   = (short)((dat[6]-'0')*10   + (dat[7]-'0'));
				}
				else
				{
						pDat  = &dat[i];
						mon   = atoi(dat);		// Month is number before that
						day   = (short) atoi(pDat+1);	// day is number after that.

						pDat  = strchr(pDat+1, *pDat); // Find next occurence of the first delimiter

						if (pDat == NULL)				// If no 2nd delim, assume current year.
								year = (short) (local_time->tm_year + 1900);
						else
								year = (short) atoi(pDat+1);
        		}
				break;
		}

      //
      //  Convert B.C.E. year to proper value!
      //

		if (strchr(dat, '.') != NULL)
				year = -year;

	
		month = (enum Months) mon;
		
		//
		//  Verify values!
		//
		if ((month <= 0) || (day <= 0))
		{
				month  	= NON_MONTH;
				day 	= 0;
				year 	= 0;
		}
		
		if ( (year > 0) && (year < 100) )
				year = year + DefaultCentury;

	}

	mdy_to_julian ();
}


//////////////////////////////////////////////////////////////
// Conversion operations
//////////////////////////////////////////////////////////////

Date::operator const char *( void ) const
{

		return (formatDate());
}

//////////////////////////////////////////////////////////////
// Date Arithmetic
//////////////////////////////////////////////////////////////


Date &Date::operator+= (long i)
{
		julian += i;

		julian_to_mdy();

		return *this;
}

//////////////////////////////////////////////////////////////

Date &Date::operator -= (long i)
{
		julian -= i;
		julian_to_mdy();

		return *this;
}

//////////////////////////////////////////////////////////////

Date Date::operator ++()
{
		julian++;

		julian_to_mdy();

		return *this;
}

//////////////////////////////////////////////////////////////

Date Date::operator --()
{
		julian--;

		julian_to_mdy();

		return *this;
}

//////////////////////////////////////////////////////////////
#if !defined(NOPOSTFIX)
Date Date::operator ++(int)
{
		Date temp=*this;				// TML - Necessary to save current
										// value of (*this) in order to
		julian++;						// simulate postfix operation!
		julian_to_mdy();

		return temp;
}

//////////////////////////////////////////////////////////////

Date Date::operator --(int)
{
		Date temp=*this;				// TML - Necessary to save current
										// value of (*this) in order to
		julian--;						// simulate postfix operation!

		julian_to_mdy();

		return temp;
}
#endif


////////////////////////////////////////////////////////////////
// Ostream operations
////////////////////////////////////////////////////////////////

ostream &operator << (ostream &os, const Date &dt)
{
	return os << dt.formatDate();
}

//////////////////////////////////////////////////////////////

ostream &operator << (ostream &os, const DOSDATE_T &dt)
{
    return os << (int)dt.month << '/' << (int)dt.day << '/' << dt.year;
}

//////////////////////////////////////////////////////////////
// Conversion routines
//////////////////////////////////////////////////////////////

void Date::julian_to_wday (void)
{
	day_of_week = (enum Wday) ((julian + 2) % 7 + 1);
}

//////////////////////////////////////////////////////////////


#define OCT5_1582		(2299160L)		// "really" 15-Oct-1582
#define OCT14_1582		(2299169L)		// "really"  4-Oct-1582
#define JAN1_1			(1721423L)

#define YEAR			(365)
#define FOUR_YEARS		(1461)
#define CENTURY 		(36524L)
#define FOUR_CENTURIES	(146097L)

void Date::julian_to_mdy ()
{
	long	z,y;
	short 	m,d;
	int 	lp;

	z = julian+1;
	if (z >= OCT5_1582)
	{
		z -= JAN1_1;
		z  = z + (z/CENTURY)  - (z/FOUR_CENTURIES) -2;
		z += JAN1_1;

	}

	z = z - ((z-YEAR) / FOUR_YEARS);		// Remove leap years before current year
	y = z / YEAR;

	d = (short) (z - (y * YEAR));

	y = y - 4712;				// our base year in 4713BC
	if (y < 1)
		y--;

	lp = !(y & 3);				// lp = 1 if this is a leap year.

	if (d==0)
	{
			y--;
			d = (short) (YEAR + lp);
	}

	m  = (short) (d/30);		// guess at month

	while (DaysSoFar[lp][m] >=d)
		m--;					// Correct guess.

	d = (short) (d - DaysSoFar[lp][m]);

	day = d;

	month = (short) (m+1);

	year = (short) y;

	julian_to_wday ();
}

//////////////////////////////////////////////////////////////


// The original here was far more complicated then it needed to be.
// What we need to keep in mind is the simple rule:
//	  Before 10/4/1585, a leap year occured every 4 years.
//	  After  10/15/1585, leap years were skipped on centuries
//		not divisible by 400. Plus 10 days were skipped to adjust
//		for the past error.

void Date::mdy_to_julian (void)
{
	int		a;
	int		work_year=year;
	long	j;
	int 	lp;

	// correct for negative year  (-1 = 1BC = year 0)

	if (work_year < 0)
			work_year++;

	lp = !(work_year & 3);			// lp = 1 if this is a leap year.

	j =
		((work_year-1) / 4)		+		// Plus ALL leap years
		DaysSoFar[lp][month-1]	+
		day					+
		(work_year * 365L)	+		// Days in years
		 JAN1_1 			+
		 -366;						// adjustments

	// deal with Gregorian calendar
	if (j >= OCT14_1582)
	{

		a = (int)(work_year/100);
		j = j+ 2 - a + a/4;			// Skip days which didn't exist.
	}

	julian = j;

	julian_to_wday ();
}


////////////////////////////////////////////////////////////////
// Format routine
////////////////////////////////////////////////////////////////

char *Date::formatDate (int type) const
{
    static char buf[40];

	strnset( buf, '\0', sizeof(buf) );

	switch ( type )
	{
		case Date::DAY:
			if ( (day_of_week < 1) || (day_of_week > 7) )
				strcpy(buf,GetString(INVALID_DAY));
			else
				strncpy( buf, GetStringArray(DAYNAME,day_of_week),
					(DisplayOptions & DATE_ABBR) ? ABBR_LENGTH : 9);
			break;

		case Date::MONTH:
			if ( (month < 1) || (month > 12) )
				strcpy(buf,GetString(INVALID_MONTH));
			else
				strncpy( buf, GetStringArray(MONTHS,month),
					(DisplayOptions & DATE_ABBR) ? ABBR_LENGTH : 9);
			break;

		case Date::FULL:
			if ( (month < 1) || (month > 12) || (day_of_week < 0) ||
				 (day_of_week > 7) )
			{
				strcpy(buf,GetString(INVALID_DATE));
			}
            else
            {
				int	y = abs(year);
				if ((DisplayOptions & NO_CENTURY)  && (y > 1899) )
						y = y % 100;
						
					strncpy( buf, GetStringArray(DAYNAME,day_of_week),
						(DisplayOptions & DATE_ABBR) ? ABBR_LENGTH : 9);

					strcat( buf, ", ");
					strncat( buf, GetStringArray(MONTHS,month),
						(DisplayOptions & DATE_ABBR) ? ABBR_LENGTH : 9);

					strcat( buf, " ");
					sprintf( buf+strlen(buf), "%d, %02d", day, y );
					if (year < 0)
						strcat(buf,szBCE);
            }
			break;

		case Date::EUROPEAN:
			if ( (month < 1) || (month > 12) || (day_of_week < 0) ||
				 (day_of_week > 7) )
			{
				strcpy(buf,GetString(INVALID_DATE));
			}
			else
            {
   				int	y = year;
				if ((DisplayOptions & NO_CENTURY)  && (y > 1899) )
						y = y % 100;
						

					sprintf(buf,"%d ",	day);

					strncat(buf, GetStringArray(MONTHS,month),
							(DisplayOptions & DATE_ABBR) ? ABBR_LENGTH : 9);

					sprintf( buf+strlen(buf), " %02d", y );

					if (year < 0)
							strcat(buf,szBCE);
            }
			break;

		case Date::COLLATE:
			if (day==0 || month==0 || year<=0)			// Can't generate COLLATE form for BCE dates.
				strcpy(buf,GetString(INVALID_DATE));
			else
				sprintf( buf, "%04d%02d%02d", year, month, day);
			break;

		case Date::MDY:
		default:
			if (day==0 || month==0 || year==0)
				strcpy(buf,GetString(INVALID_DATE));
			else
			{
				int	y = year;
				if ((DisplayOptions & NO_CENTURY)  && (y > 1899) )
						y = y % 100;
						
				sprintf( buf+strlen(buf), "%1d/%1d/%02d", month, day, y);
			}
			break;
	}

    return (buf);
}

//////////////////////////////////////////////////////////////


int Date::setCentury(short century)
{
	int		oldcent = DefaultCentury;
	              
	if ( (century > 0) && (century < 100) )
			century = century * 100;
		              
	DefaultCentury = century;
	
	return(oldcent);
}	

//////////////////////////////////////////////////////////////

bool Date::setOption( int option, int action )
{
		bool		retv = true;

		switch ( option )
		{
			case NO_CENTURY:
				if ( action )
						DisplayOptions |= NO_CENTURY;
				else
						DisplayOptions &= (~NO_CENTURY);
				break;

			case DATE_ABBR:
				if ( action )
						DisplayOptions |= DATE_ABBR;
				else
						DisplayOptions &= (~DATE_ABBR);
				break;

			default:
				retv = false;
				break;
		}

    	return(retv);
}

///////////////////////////////////////////////////////////////
//  Miscellaneous Routines
///////////////////////////////////////////////////////////////


PUBLIC int Date::DOY( void ) const
{
	Date temp( 1, 1, year );

	return (int) (julian - temp.julian + 1);
}


//////////////////////////////////////////////////////////////

PUBLIC int Date::isLeapYear( void ) const
{
	return  ( (year >= 1582) ?
			  (year % 4 == 0  &&  year % 100 != 0  ||  year % 400 == 0 ):
			  (year % 4 == 0) );
}

//////////////////////////////////////////////////////////////
PUBLIC bool Date::isDST( void ) const
{
    // Initialize start of DST and STD
    Date tempDST( (startDST >> 8), (startDST & 255), year ) ;
    Date tempSTD( (startSTD >> 8), (startSTD & 255), year ) ;

    tempDST += -tempDST.NDOW() + 8 ;    // DST begins first Sunday in April
    tempSTD -= (tempSTD.NDOW() - 1) ;   // STD begins last Sunday in October

    return( (bool) (julian >= tempDST.julian && julian < tempSTD.julian) ) ;
}
//////////////////////////////////////////////////////////////

PUBLIC bool Date::setDST(unsigned nMonth, unsigned nDay)
{
    if (nMonth < 13 && nDay < 32)
    {
        startDST = (nMonth << 8) + nDay;
        return true;
    }
    else
        return false;
}
//////////////////////////////////////////////////////////////

PUBLIC bool Date::setSTD(unsigned nMonth, unsigned nDay)
{
    if (nMonth < 13 && nDay < 32)
    {
        startSTD = (nMonth << 8) + nDay;
        return true;
    }
    else
        return false;
}

//////////////////////////////////////////////////////////////

#if defined (MSDOS) || defined(_WIN32)
PUBLIC DOSDATE_T Date::eom( void ) const
{
	static DOSDATE_T eom_temp;

	Date tempdate( (short) ((month % 12) + 1), 1, year);

	if (month == 12)
		tempdate.year++;

	tempdate--;

	eom_temp.year  = (int)	         tempdate.year;
	eom_temp.month = (unsigned char) tempdate.month;
	eom_temp.day   = (unsigned char) tempdate.day;

	return eom_temp;
}


PUBLIC DOSDATE_T Date::getDate( void ) const
{
	static DOSDATE_T getDate_temp;

	getDate_temp.year  = 		year;
	getDate_temp.month = (unsigned char) month;
	getDate_temp.day   = (unsigned char) day;

	return getDate_temp;
}

#endif

//////////////////////////////////////////////////////////////

//-------------------------------------------------
// Version 4.0 Extension to Public Interface - CDP
//-------------------------------------------------

PUBLIC Date &Date::Set(void)
{
	tm			*local_time;
	time_t		timer;

	timer 	  = time(NULL);
	local_time = localtime(&timer);

	month = (short) (local_time->tm_mon + 1);
	day   = (short)  local_time->tm_mday;
	year  = (short) (local_time->tm_year + 1900);

	mdy_to_julian();

	return *this;
}

//////////////////////////////////////////////////////////////

PUBLIC Date &Date::Set(unsigned int	nMonth,unsigned int	nDay,unsigned int	nYear)
{
	month = (short) nMonth;
	year  = (short) (nYear > 9999 ? 0 : nYear);

	day   = (short) ((nDay < DaysInMonth()) ? nDay : DaysInMonth());

	mdy_to_julian();

	return *this;
}

//////////////////////////////////////////////////////////////

PUBLIC Date &Date::Set(long	j)
{
	julian = j;

	julian_to_mdy();

	return *this;
}

//////////////////////////////////////////////////////////////

PUBLIC Date &Date::Set(int weeknum, int dow, short m, short y)
{
		int	d;

		assert(weeknum > -1);		// weeknum = 0 mean "the last" 
		assert(weeknum < 6);
		assert(dow >  NON_DAY);
		assert(dow <= SATURDAY);
		assert(m   >  NON_MONTH);
		assert(m   <= DECEMBER);

		Set(m,1,y);

		mdy_to_julian ();

        if (weeknum ==0)	
		{
				AddMonths(1);
		}

		d = (dow - day_of_week);
		julian = julian + d;

		if (d > 0)
			weeknum--;
			
		AddWeeks(weeknum);			// Julian_to_mdy done in AddWeeks()

		return(*this);		
}

//////////////////////////////////////////////////////////////

PUBLIC unsigned int Date::DaysInMonth() const
{
    return (unsigned int) (GauDays[month] + (month==2 && isLeapYear()));
}

//////////////////////////////////////////////////////////////

PUBLIC Date &Date::AddMonths(int nCount)
{
    int nDays;

	month += (short) nCount;

	while (month < 1)
	{
        month += 12;
        year--;
	}

	while (month > 12)
	{
        month -= 12;
        year++;
	}

    nDays = (unsigned int) DaysInMonth();

    if (day > nDays)                    // Bump up the month by one if day is
    {                                   // greater than # of days in new month
        month++;                        // and assigned the difference as the
        day -= nDays;                   // day value for the new month! - TML
    }

	mdy_to_julian();

	return *this;
}

//////////////////////////////////////////////////////////////

PUBLIC Date & Date::AddWeeks(int nCount)
{
    Set(julian + (long)nCount*7);
    return *this;
}


//////////////////////////////////////////////////////////////

PUBLIC Date &Date::AddYears(int nCount)
{
    year += (short) nCount;
    mdy_to_julian();

    return *this;
}

//////////////////////////////////////////////////////////////

PUBLIC int Date::WOM(void) const 
{
	// Abs day includes the days from previous month that fills up
	// the begin. of the week.

    int nAbsDay = day + FirstDOM()-1;
    return (nAbsDay-NDOW())/7 + 1;
}
//////////////////////////////////////////////////////////////


PUBLIC int Date::WOY(void) const 
{
    Date   doTemp(1, 1, year);
    return (int)(((julian - doTemp.julian+1)/7) + 1);
}


