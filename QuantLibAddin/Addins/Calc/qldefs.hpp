#define CSS					::com::sun::star
#define SEQ(c)				CSS::uno::Sequence< c >
#define SEQSEQ(c)           CSS::uno::Sequence< CSS::uno::Sequence< c > >
#define STRING				::rtl::OUString
#define STRFROMANSI(s)		STRING( s, strlen( s ), RTL_TEXTENCODING_MS_1252 )
#define STRFROMASCII(s)		STRING::createFromAscii( s )
#define THROWDEF_RTE		throw(CSS::uno::RuntimeException)
#define THROWDEF_RTE_IAE	throw(CSS::uno::RuntimeException,CSS::lang::IllegalArgumentException)
#define THROW_RTE			throw CSS::uno::RuntimeException()
#define REF(c)				CSS::uno::Reference< c >
#define ANY					CSS::uno::Any
