/*
*+----------------------------------------------------------------------
*| Header.......: DATECL.H
*| Date.........: Sat  12-01-1994
*| Author.......: James M. Curran,  et al
*| Version......: 5.0   Compile w/MSC++ 7.0 or Borland C++ 3.1 (or later versions)
*| Usage........: General purpose date conversion, arithmetic,
*|              :    comparison, and formatting class
*| Compile line.: cl /AM /W3 /Zp /D_DOS /Osel /Gs /c datecl4.cpp
*|              : cl /AM /W3 /Zp /D_DOS /Osel /Gs /c datedemo.cpp
*| Link line....:
*|    link /NOD /ONERROR:NOEXE datedemo date, datedemo, NUL, mafxcr mlibce;
*|
*| Acknowledgements:
*|
*|    Originally inspired by Steve Marcus (CIS 72007,1233)  6/16/91
*|    Enhanced by Eric Simon (CIS 70540,1522)               6/29/91
*|    Further Enhanced by Chris Hill (CIS 72030,2606)       7/11/91
*|    Still Further Enhanced by Hill & Simon  v3.10         8/05/91
*|
*|    "It jist keeps on a 'git 'n bedder!"
*|       by Charles D. Price (CIS 70541,3651) v4.0          6/27/92
*|
*|     Sealing the memory leaks...
*|         some variable casts and string output.
*|             by Kenneth A. Argo (CIS 71241,3635) v4.1        3/10/93
*|
*|     "Yet, more improvements..."
*|             by Ly Minh Tr¡ (CIS 73062,512)  v4.2            3/13/93
*|             ............................... v4.3            3/24/93
*|             ............................... v4.4            6/03/93
*|             ............................... v4.5            6/21/93
*|             ............................... v4.6            8/04/93
*|             ............................... v4.7            9/20/93
*|             ............................... v4.8           11/18/93
*|             ............................... v4.9            1/26/94
*|
*|      "All kinds of good stuff..."
*|			   by James M. Curran (CIS 72261,655)  v5.0 	  10/30/94
*|
*|
*|    And the quest for the perfect date class continues....
*|
*+----------------------------------------------------------------------
*/
#ifndef __cplusplus
#error  Requires C++ Compiler
#endif

#ifndef DATECLS_H
#define DATECLS_H

//---------------------- Compatibility Section -----------------------------------------
//
// Here we attempt to smooth out all the variations between different compiliers, by 
// #define-ing several symbols to include or remove, or to use a common name.
//
// The #defines used are :
//
// #define MSDOS		// If target system is MS-DOS based.
// #define DOSDATE_T	// name of the dos_date struct, (need only when "MSDOS" is defined)
// #define NOPOSTFIX	// If compiler cannot handle postfix ++.
// #define f_EXISTS // if a boolean type of one form or another already exists.
//


#if !defined(BOOLEAN_EXISTS)
	// this simulates (poorly) the new "bool" basic type as defined in the
	// ANSI/ISO C++ committee working papers, where "bool", "true" and "false"
	// are new keywords.   When the standard is finalized, this entire #if/#endif 
	// can be deleted.
	//typedef enum __booltag {false, true} bool;
#endif


#if defined(__MSDOS__)	// Borland uses "__MSDOS__" while Microsoft uses
	#define	MSDOS	TRUE	// "MSDOS".  I'm not sure what WATCOM, et al use, 
#endif						// but I figure this should cover most of 'em.



#if defined (__BORLANDC__)  || defined (__TURBOC__)
		#define	DOSDATE_T	dosdate_t
		#include <iostream.h>
#elif defined(_MSC_VER) 
		#if defined(_WIN32)
		typedef struct _dosdate_t
			{							// Current date structure
			unsigned char day;			// Day of the month: 1-31
			unsigned char month;		// Month of the year: 1-12
			unsigned int year;			// Year: 0-119 relative to 1980
			unsigned char dayofweek;	// Day of the week: 0-6 (Sunday is 0)
			} _dosdate_t;
		#endif
		#define DOSDATE_T   _dosdate_t
		#include <iostream.h>

#elif defined (__ZTC__)  && __ZTC__ < 0x0600

		#define	DOSDATE_T	dos_date_t
		#define	MSDOS			TRUE
		#define NOPOSTFIX
		#include <stream.hpp>

#elif defined (__ZTC__) || defined(__SC__)

		#define	DOSDATE_T	dos_date_t
		#define	MSDOS			TRUE
		#include <iostream.h>

#elif	defined (___WATCOMC__)
		#error Add #defines for Watcom C
		#define	NON_MSDOS	TRUE			// Until #defines are added.

#else		// Add other compilers here

		#define	NON_MSDOS	TRUE
        //#include <iostream.h>
        #include <iostream>

#endif

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#if defined(MSDOS)
	#include <dos.h>
#endif
#if defined(_WIN32)
	#include <windows.h>
#endif

#define PUBLIC           // just a couple of friendly reminders!
#define MEMBER

#define ABBR_LENGTH 3


class Date
{
	public:
        //
        // TML - Put into class so we don't proliferate global names...in the
        //       tradition of the 'ios' class!
        //       Make use of the encapsulation feature of C++
        //
        enum format_type {MDY, DAY, MONTH, FULL, EUROPEAN, COLLATE};
        enum {OFF, ON};
        enum {BUF_SIZE=40};
        enum Actions { NO_CENTURY  = 0x02,
                     		 DATE_ABBR   = 0x04};
        enum Wday 	{NON_DAY=0, SUNDAY=1,MONDAY,TUESDAY,WEDNESDAY,THURSDAY,FRIDAY,SATURDAY};
        enum Months   {NON_MONTH=0, JANUARY=1, FEBRUARY, MARCH, APRIL, MAY, JUNE, JULY, AUGUST,
								SEPTEMBER, OCTOBER, NOVEMBER, DECEMBER};
	//protected:
		unsigned long julian;	// see julDate();  days since 1/1/4713 B.C.
		short 	year;			// see NYear4()
		short	month;			// see NMonth()
		short 	day;			// see Day()
		char 	day_of_week;	// see NDOW();	1 = Sunday, ... 7 = Saturday
		char	separator;

	private:
		static			int		DisplayFormat;
		static unsigned int		DisplayOptions;
		static unsigned short	DefaultCentury;
		static unsigned int 		startDST;
		static unsigned int 		startSTD;


		void julian_to_mdy ();         // convert julian day to mdy
		void julian_to_wday ();        // convert julian day to day_of_week
		void mdy_to_julian ();         // convert mdy to julian day

	public:
		Date ();
		Date (short m, short d, short y);
		Date (long 	j);
		Date (const char 	*dat);
		Date (const Date 	&dt);
		Date (const tm	 	&TM);
		Date (int weeknum, int dow, short m, short y);
        inline void writeDate(Date d) { std::cout << d << std::endl; }
#if defined (MSDOS) || defined(_WIN32)
		Date (const DOSDATE_T &ds);
#endif

        virtual ~Date() {}              // Do nothing!

        operator const char *( void ) const;        // Date to character - via type casting

        inline Date operator + (long i)	const	{return Date(julian + i);}
        inline Date operator + (int i)	const	{return Date(julian + (long)i);}
        inline Date operator - (long i)	const 	{return Date(julian - i);}
        inline Date operator - (int i)	const	{return Date(julian - (long)i);}
        inline long operator - (const Date &dt)	const	{return ( julian - dt.julian );}

        Date &operator += (long i);
        Date &operator -= (long i);

        Date  operator ++ ();               // Prefix increment
        Date  operator -- ();               // Prefix decrement

#if !defined(NOPOSTFIX)
        Date  operator ++ (int);            // Postfix increment
        Date  operator -- (int);            // Postfix decrement
#endif
        inline int operator <  (const Date &dt) const {return(julian <  dt.julian);}
        inline int operator <= (const Date &dt) const {return(julian <= dt.julian);}
        inline int operator >  (const Date &dt) const {return(julian >  dt.julian);}
        inline int operator >= (const Date &dt) const {return(julian >= dt.julian);}
        inline int operator == (const Date &dt) const {return(julian == dt.julian);}
        inline int operator != (const Date &dt) const {return(julian != dt.julian);}

        inline 	static void  	setFormat (enum format_type format)	{DisplayFormat = format;}
				static bool		setOption (int option, int action=ON);
				static int   	setCentury(short century);

        friend std::ostream &operator << (std::ostream &os, enum format_type ft) {Date::setFormat(ft); return(os);}

#if defined(MSDOS) || defined(_WIN32)
        friend std::ostream &operator << (std::ostream &os, const DOSDATE_T &dt);
#endif

		char *formatDate(int type=DisplayFormat) const;

inline long	julDate()		const	{return(julian);}	// returns julian date
		int	DOY()			const;						// returns relative date since Jan. 1

		int		isLeapYear()	const;		// returns true if leap year, false if not
		bool 	isDST()			const;		// returns true if date is within Daylight
                                   			// Savings Time (DST), false if not

        // Sets the month and day which DST and STD date begins!  This will
        // enable isDST() to return the correct result for regions other than
        // North America.  Returns true if month and day values are valid, false
        // otherwise - TML

		static bool	setDST(unsigned nMonth, unsigned nDay);
		static bool	setSTD(unsigned nMonth, unsigned nDay);


#if defined (MSDOS) || defined(_WIN32)
		// note that the next functions return a date struct as defined in
		// dos.h (distinct from the Date class)
		DOSDATE_T	eom()		const;  // returns last day of month in object
		DOSDATE_T	getDate()	const;  // returns a date structure

#endif
		//-------------------------------------------------
		// Version 4.0 Extension to Public Interface - CDP
		//-------------------------------------------------

		// These 'Set's modify the date object and actually SET it.
		// They all return a reference to self (*this)

			Date &Set(void);       // Sets to current system date
			Date &Set(long lJulian);
			Date &Set(unsigned int nMonth, unsigned int nDay, unsigned int nYear);
			Date &Set(int weeknum, int dow, short m, short y);
			Date &AddMonths(int nCount = 1); // May also pass neg# to decrement

			Date &AddWeeks(int nCount = 1);	//
			Date &AddYears(int nCount = 1);	//

			unsigned int DaysInMonth(void) const ;	// Number of days in month (1..31)

			int	WOM(void) 		const;	// Numeric Week Of Month  (1..6)
			int	WOY(void) 		const;	// Numeric Week Of Year  (1..52)

										// First Day Of Month	(1..7)
	inline	int	FirstDOM(void)	const	{return Date(month, 1, year).NDOW();}

										// Numeric Day of date object
	inline	int Day(void)		const	{return day;}

										//	Day Of Week
										// Character ('Sunday'..'Saturday')
	inline	const char * CDOW(void)	const	{return(formatDate(DAY));}
										// (1..7)
	inline	int	NDOW(void)	const	{return day_of_week;}

									// eg. 1992
	inline	int NYear4()	const	{return year;}

									// Month Number (1..12)
	inline	int NMonth()	const	{return month;}

									// First Date Of Month
	inline	Date BOM()		const 	{return(Date(month, 1, year));}

									// Last Date Of Month
	inline	Date EOM()		const 	{return((Date(month, 1, year).AddMonths(1))-1);}

									// First Date Of Year
	inline	Date BOY()		const	{return(Date(1, 1, year));}

									// Last Date Of Year
	inline	Date EOY()		const	{return(Date(1, 1, year+1)-1);}

									// Character Month name
	inline	const char * CMonth() const	{return(formatDate(MONTH));}

#ifndef NO_HOLIDAYS
	inline  static Date	NewYearsDay(short year)	{return(Date(JANUARY, 1, year));}
	inline  static Date	ValentinesDay(short year)	{return(Date(FEBRUARY, 14, year));}
	inline  static Date	PresidentsDay(short year)	{return(Date(3, MONDAY, FEBRUARY, year));}
	inline  static Date	StPatricksDay(short year)	{return(Date(MARCH, 17, year));}
	inline  static Date	MothersDay(short year)	{return(Date(2, SUNDAY, MAY, year));}
	inline  static Date	MemorialDay(short year)	{return(Date(0, MONDAY, MAY, year));}
	inline  static Date	FlagDay(short year)		{return(Date(JUNE, 14, year));}
	inline  static Date	FathersDay(short year)	{return(Date(3, SUNDAY, JUNE, year));}
	inline  static Date	CanadaDay(short year)		{return(Date(JULY, 1, year));}
	inline  static Date	IndependenceDay(short year)	{return(Date(JULY, 4, year));}
	inline  static Date	BastilleDay(short year)	{return(Date(JULY, 14, year));}
	inline  static Date	LaborDay(short year)		{return(Date(1, MONDAY, SEPTEMBER, year));}
	inline  static Date	VeteransDay(short year)	{return(Date(NOVEMBER, 11, year));}
	inline  static Date	ThanksgivingDay(short year)	{return(Date(4, THURSDAY, NOVEMBER, year));}
	inline  static Date	ChristmasDay(short year)	{return(Date(DECEMBER, 25, year));}
#endif	// NO_HOLIDAYS

};

#endif
