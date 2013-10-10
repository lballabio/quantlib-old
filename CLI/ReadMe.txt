see https://sourceforge.net/mailarchive/message.php?msg_id=29716902.

and

http://connect.microsoft.com/VisualStudio/feedback/details/336844/static-variable-in-native-method-causes-exception-c0020001-during-process-exit


As a workaround:

copy africa.hpp/cpp; america.hpp/cpp; asia.hpp/cpp; europe.hpp/cpp; and oceania.hpp/cpp 
to ql/currencies folder.

copy singleton.hpp to ql/patterns folder.

rebuild QuantLib.

