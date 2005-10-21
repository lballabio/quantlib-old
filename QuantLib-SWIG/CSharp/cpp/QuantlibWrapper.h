// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the QUANTLIBWRAPPER_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// QUANTLIBWRAPPER_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
#ifdef QUANTLIBWRAPPER_EXPORTS
#define QUANTLIBWRAPPER_API __declspec(dllexport)
#else
#define QUANTLIBWRAPPER_API __declspec(dllimport)
#endif

// This class is exported from the QuantlibWrapper.dll
class QUANTLIBWRAPPER_API CQuantlibWrapper {
public:
	CQuantlibWrapper(void);
	// TODO: add your methods here.
};

extern QUANTLIBWRAPPER_API int nQuantlibWrapper;

QUANTLIBWRAPPER_API int fnQuantlibWrapper(void);
