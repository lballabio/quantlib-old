//f_begin name=QuantLib::close
//f_begin name=SimpleQuote
//f_begin name=value
//f_begin name=~SimpleQuote
#include "QuantLibAddin.hpp"
#include <oh/objecthandler.hpp>

#include <ql/math/comparison.hpp>
#include <ql/quotes/simplequote.hpp>

//f_header name=QuantLib::close
//f_header name=SimpleQuote
//f_header name=value
//f_header name=~SimpleQuote
//module=QuantLibAddin
//XXX***functionWrapper*******
/* 0 name QuantLib::close */
/* 1 sym:symtab Hash(0x2b7ea66e89f0) {
  'name' : QuantLib, 
  'parentNode' : Hash(0x2b7ea66d2850) {.....}, 
  'csymtab' : Hash(0x2b7ea66e8a30) {..}, 
  'nodeType' : symboltable, 
  'symtab' : Hash(0x2b7ea66e89d0) {..}, 
  'firstChild' : Hash(0x2b7ea66e91d0) {.....}, 
  'lastChild' : Hash(0x2b7ea66e91d0) {.....}, 
} */
/* 2 nodeType cdecl */
/* 3 view globalfunctionHandler */
/* 4 nextSibling Hash(0x2b7ea66e9070) {
  'name' : QuantLib::SimpleQuote, 
  'sym:symtab' : Hash(0x2b7ea66e89f0) {.......}, 
  'symtab' : Hash(0x2b7ea66e91d0) {.....}, 
  'nodeType' : class, 
  'allows_typedef' : 1, 
  'typepass:visit' : 1, 
  'allocate:visit' : 1, 
  'kind' : class, 
  'sym:name' : SimpleQuote, 
  'previousSibling' : Hash(0x2b7ea66e8dd0) {...............}, 
  'lastChild' : Hash(0x2b7ea66e9b10) {............}, 
  'firstChild' : Hash(0x2b7ea66e9170) {....}, 
  'feature:featurename' : 1, 
  'allocate:default_constructor' : 1, 
  'allocate:has_constructor' : 1, 
  'allocate:public_constructor' : 1, 
  'allocate:copy_constructor' : 1, 
  'allocate:default_destructor' : 1, 
  'parentNode' : Hash(0x2b7ea66e8a90) {............}, 
  'privatebaselist' : List[  ], 
  'protectedbaselist' : List[  ], 
  'baselist' : List[ Quote ], 
  'module' : Hash(0x2b7ea66e86b0) {....}, 
  'sym:overname' : __SWIG_0, 
  'typescope' : Hash(0x2b7ea66ea510) {.....}, 
} */
/* 5 kind function */
/* 6 sym:name close */
/* 7 globalfunctionHandler:parms Hash(0x2b7ea66e8bf0) {
  'name' : x, 
  'type' : Real, 
  'nextSibling' : Hash(0x2b7ea66e8d10) {....}, 
  'nodeType' : parm, 
  'lname' : arg1, 
} */
/* 8 feature:featurename 1 */
/* 9 parentNode Hash(0x2b7ea66e8a90) {
  'feature:featurename' : 1, 
  'sym:name' : QuantLib, 
  'name' : QuantLib, 
  'previousSibling' : Hash(0x2b7ea66e8850) {..............}, 
  'typescope' : Hash(0x2b7ea66ea0f0) {.....}, 
  'parentNode' : Hash(0x2b7ea66e8530) {........}, 
  'nodeType' : namespace, 
  'firstChild' : Hash(0x2b7ea66e8dd0) {...............}, 
  'lastChild' : Hash(0x2b7ea66e9070) {.........................}, 
  'symtab' : Hash(0x2b7ea66e89f0) {.......}, 
  'sym:symtab' : Hash(0x2b7ea66d2850) {.....}, 
  'sym:overname' : __SWIG_0, 
} */
/* 10 decl f(Real,Real). */
/* 11 parms Hash(0x2b7ea66e8bf0) {
  'name' : x, 
  'type' : Real, 
  'nextSibling' : Hash(0x2b7ea66e8d10) {....}, 
  'nodeType' : parm, 
  'lname' : arg1, 
} */
/* 12 wrap:action result = (bool)QuantLib::close(arg1,arg2); */
/* 13 type bool */
/* 14 sym:overname __SWIG_0 */
/* 0 name x */
/* 1 type Real */
/* 2 nextSibling Hash(0x2b7ea66e8d10) {
  'name' : y, 
  'type' : Real, 
  'nodeType' : parm, 
  'lname' : arg2, 
} */
/* 3 nodeType parm */
/* 4 lname arg1 */
/* 0 name y */
/* 1 type Real */
/* 2 nodeType parm */
/* 3 lname arg2 */
//*************
//module=QuantLibAddin
//XXX***functionWrapper*******
/* 0 constructorHandler:view constructorDeclaration */
/* 1 name SimpleQuote */
/* 2 ismember 1 */
/* 3 sym:symtab Hash(0x2b7ea66e91d0) {
  'name' : SimpleQuote, 
  'parentNode' : Hash(0x2b7ea66e89f0) {.......}, 
  'csymtab' : Hash(0x2b7ea66e9230) {...}, 
  'nodeType' : symboltable, 
  'symtab' : Hash(0x2b7ea66e91b0) {...}, 
} */
/* 4 nodeType constructor */
/* 5 sym:nextSibling Hash(0x2b7ea66e97d0) {
  'name' : SimpleQuote, 
  'ismember' : 1, 
  'sym:symtab' : Hash(0x2b7ea66e91d0) {.....}, 
  'nodeType' : constructor, 
  'nextSibling' : Hash(0x2b7ea66e9b10) {.............}, 
  'sym:previousSibling' : Hash(0x2b7ea66e9690) {.........................}, 
  'sym:name' : SimpleQuote, 
  'feature:featurename' : 1, 
  'defaultargs' : Hash(0x2b7ea66e9690) {.........................}, 
  'decl' : f()., 
  'parentNode' : Hash(0x2b7ea66e9070) {..............................}, 
  'access' : public, 
  'sym:overloaded' : Hash(0x2b7ea66e9690) {.........................}, 
  'sym:overname' : __SWIG_1, 
  'feature:new' : 1, 
} */
/* 6 csym:nextSibling Hash(0x2b7ea66e97d0) {
  'name' : SimpleQuote, 
  'ismember' : 1, 
  'sym:symtab' : Hash(0x2b7ea66e91d0) {.....}, 
  'nodeType' : constructor, 
  'nextSibling' : Hash(0x2b7ea66e9b10) {.............}, 
  'sym:previousSibling' : Hash(0x2b7ea66e9690) {.........................}, 
  'sym:name' : SimpleQuote, 
  'feature:featurename' : 1, 
  'defaultargs' : Hash(0x2b7ea66e9690) {.........................}, 
  'decl' : f()., 
  'parentNode' : Hash(0x2b7ea66e9070) {..............................}, 
  'access' : public, 
  'sym:overloaded' : Hash(0x2b7ea66e9690) {.........................}, 
  'sym:overname' : __SWIG_1, 
  'feature:new' : 1, 
} */
/* 7 sym:name new_SimpleQuote */
/* 8 nextSibling Hash(0x2b7ea66e97d0) {
  'name' : SimpleQuote, 
  'ismember' : 1, 
  'sym:symtab' : Hash(0x2b7ea66e91d0) {.....}, 
  'nodeType' : constructor, 
  'nextSibling' : Hash(0x2b7ea66e9b10) {.............}, 
  'sym:previousSibling' : Hash(0x2b7ea66e9690) {.........................}, 
  'sym:name' : SimpleQuote, 
  'feature:featurename' : 1, 
  'defaultargs' : Hash(0x2b7ea66e9690) {.........................}, 
  'decl' : f()., 
  'parentNode' : Hash(0x2b7ea66e9070) {..............................}, 
  'access' : public, 
  'sym:overloaded' : Hash(0x2b7ea66e9690) {.........................}, 
  'sym:overname' : __SWIG_1, 
  'feature:new' : 1, 
} */
/* 9 constructorDeclaration:sym:name SimpleQuote */
/* 10 view constructorHandler */
/* 11 constructorHandler:sym:name SimpleQuote */
/* 12 feature:featurename 1 */
/* 13 constructorHandler:type  */
/* 14 constructorHandler:parms Hash(0x2b7ea66e94f0) {
  'name' : value, 
  'value' : Null< Real >(), 
  'type' : Real, 
  'nodeType' : parm, 
} */
/* 15 decl f(Real). */
/* 16 parentNode Hash(0x2b7ea66e9070) {
  'classtype' : QuantLib::SimpleQuote, 
  'name' : QuantLib::SimpleQuote, 
  'sym:symtab' : Hash(0x2b7ea66e89f0) {.......}, 
  'symtab' : Hash(0x2b7ea66e91d0) {.....}, 
  'nodeType' : class, 
  'allows_typedef' : 1, 
  'typepass:visit' : 1, 
  'allocate:visit' : 1, 
  'kind' : class, 
  'sym:name' : SimpleQuote, 
  'previousSibling' : Hash(0x2b7ea66e8dd0) {..............}, 
  'lastChild' : Hash(0x2b7ea66ec790) {..........}, 
  'firstChild' : Hash(0x2b7ea66e9170) {....}, 
  'feature:featurename' : 1, 
  'allocate:default_constructor' : 1, 
  'allocate:has_constructor' : 1, 
  'allocate:public_constructor' : 1, 
  'allocate:copy_constructor' : 1, 
  'allocate:default_destructor' : 1, 
  'has_destructor' : 1, 
  'allocate:destructor' : 1, 
  'sym:cleanconstructor' : 1, 
  'parentNode' : Hash(0x2b7ea66e8a90) {............}, 
  'classtypeobj' : QuantLib::SimpleQuote, 
  'privatebaselist' : List[  ], 
  'protectedbaselist' : List[  ], 
  'baselist' : List[ Quote ], 
  'module' : Hash(0x2b7ea66e86b0) {....}, 
  'sym:overname' : __SWIG_0, 
  'typescope' : Hash(0x2b7ea66ea510) {.....}, 
} */
/* 17 access public */
/* 18 sym:overloaded Hash(0x2b7ea66e9690) {
  'constructorHandler:view' : constructorDeclaration, 
  'name' : SimpleQuote, 
  'ismember' : 1, 
  'sym:symtab' : Hash(0x2b7ea66e91d0) {.....}, 
  'nodeType' : constructor, 
  'sym:nextSibling' : Hash(0x2b7ea66e97d0) {...............}, 
  'csym:nextSibling' : Hash(0x2b7ea66e97d0) {...............}, 
  'sym:name' : new_SimpleQuote, 
  'nextSibling' : Hash(0x2b7ea66e97d0) {...............}, 
  'constructorDeclaration:sym:name' : SimpleQuote, 
  'view' : constructorHandler, 
  'constructorHandler:sym:name' : SimpleQuote, 
  'feature:featurename' : 1, 
  'constructorHandler:type' : <Object 'VoidObj' at 0x2b7ea66d2010>, 
  'constructorHandler:parms' : Hash(0x2b7ea66e94f0) {....}, 
  'decl' : f(Real)., 
  'parentNode' : Hash(0x2b7ea66e9070) {..............................}, 
  'access' : public, 
  'sym:overloaded' : Hash(0x2b7ea66e9690), 
  'parms' : Hash(0x2b7ea66ecb90) {.....}, 
  'wrap:action' : result = (QuantLib::SimpleQuote *)new QuantLib::SimpleQuote(arg1);, 
  'type' : p.QuantLib::SimpleQuote, 
  'sym:overname' : __SWIG_0, 
  'feature:new' : 1, 
  'constructorHandler:name' : SimpleQuote, 
} */
/* 19 parms Hash(0x2b7ea66ecb90) {
  'name' : value, 
  'value' : Null< Real >(), 
  'type' : Real, 
  'nodeType' : parm, 
  'lname' : arg1, 
} */
/* 20 wrap:action result = (QuantLib::SimpleQuote *)new QuantLib::SimpleQuote(arg1); */
/* 21 type p.QuantLib::SimpleQuote */
/* 22 sym:overname __SWIG_0 */
/* 23 feature:new 1 */
/* 24 constructorHandler:name SimpleQuote */
/* 0 name value */
/* 1 value Null< Real >() */
/* 2 type Real */
/* 3 nodeType parm */
/* 4 lname arg1 */
//*************
//module=QuantLibAddin
//XXX***functionWrapper*******
/* 0 name value */
/* 1 ismember 1 */
/* 2 sym:symtab Hash(0x2b7ea66e91d0) {
  'name' : SimpleQuote, 
  'parentNode' : Hash(0x2b7ea66e89f0) {.......}, 
  'csymtab' : Hash(0x2b7ea66e9230) {...}, 
  'nodeType' : symboltable, 
  'symtab' : Hash(0x2b7ea66e91b0) {...}, 
} */
/* 3 nodeType cdecl */
/* 4 nextSibling Hash(0x2b7ea66ec8d0) {
  'kind' : public, 
  'previousSibling' : Hash(0x2b7ea66e9b10) {......................}, 
  'nextSibling' : Hash(0x2b7ea66ec790) {..........}, 
  'parentNode' : Hash(0x2b7ea66e9070) {...............................}, 
  'nodeType' : access, 
} */
/* 5 kind function */
/* 6 sym:name SimpleQuote_value */
/* 7 memberfunctionHandler:sym:name value */
/* 8 view memberfunctionHandler */
/* 9 feature:featurename 1 */
/* 10 memberfunctionHandler:type Real */
/* 11 memberfunctionHandler:parms  */
/* 12 parentNode Hash(0x2b7ea66e9070) {
  'classtype' : QuantLib::SimpleQuote, 
  'name' : QuantLib::SimpleQuote, 
  'sym:symtab' : Hash(0x2b7ea66e89f0) {.......}, 
  'symtab' : Hash(0x2b7ea66e91d0) {.....}, 
  'nodeType' : class, 
  'allows_typedef' : 1, 
  'typepass:visit' : 1, 
  'allocate:visit' : 1, 
  'kind' : class, 
  'sym:name' : SimpleQuote, 
  'previousSibling' : Hash(0x2b7ea66e8dd0) {..............}, 
  'lastChild' : Hash(0x2b7ea66ec790) {..........}, 
  'firstChild' : Hash(0x2b7ea66e9170) {....}, 
  'feature:featurename' : 1, 
  'allocate:default_constructor' : 1, 
  'allocate:has_constructor' : 1, 
  'allocate:public_constructor' : 1, 
  'allocate:copy_constructor' : 1, 
  'allocate:default_destructor' : 1, 
  'has_destructor' : 1, 
  'allocate:destructor' : 1, 
  'sym:cleanconstructor' : 1, 
  'has_constructor' : 1, 
  'parentNode' : Hash(0x2b7ea66e8a90) {............}, 
  'classtypeobj' : QuantLib::SimpleQuote, 
  'privatebaselist' : List[  ], 
  'protectedbaselist' : List[  ], 
  'baselist' : List[ Quote ], 
  'module' : Hash(0x2b7ea66e86b0) {....}, 
  'sym:overname' : __SWIG_0, 
  'typescope' : Hash(0x2b7ea66ea510) {.....}, 
} */
/* 13 decl q(const).f(). */
/* 14 access public */
/* 15 parms Hash(0x2b7ea66ed0f0) {
  'name' : self, 
  'type' : p.q(const).QuantLib::SimpleQuote, 
  'self' : 1, 
  'hidden' : 1, 
  'nodeType' : parm, 
  'lname' : arg1, 
} */
/* 16 wrap:action result = (Real)((QuantLib::SimpleQuote const *)arg1)->value(); */
/* 17 type Real */
/* 18 qualifier q(const). */
/* 19 sym:overname __SWIG_0 */
/* 20 memberfunctionHandler:value  */
/* 21 memberfunctionHandler:name value */
/* 0 name self */
/* 1 type p.q(const).QuantLib::SimpleQuote */
/* 2 self 1 */
/* 3 hidden 1 */
/* 4 nodeType parm */
/* 5 lname arg1 */
//*************
//module=QuantLibAddin
//XXX***functionWrapper*******
/* 0 destructorHandler:view destructorDeclaration */
/* 1 name ~SimpleQuote */
/* 2 sym:symtab Hash(0x2b7ea66e91d0) {
  'name' : SimpleQuote, 
  'parentNode' : Hash(0x2b7ea66e89f0) {.......}, 
  'csymtab' : Hash(0x2b7ea66e9230) {...}, 
  'nodeType' : symboltable, 
  'symtab' : Hash(0x2b7ea66e91b0) {...}, 
} */
/* 3 nodeType destructor */
/* 4 view destructorHandler */
/* 5 previousSibling Hash(0x2b7ea66ec8d0) {
  'kind' : public, 
  'previousSibling' : Hash(0x2b7ea66e9b10) {................}, 
  'nextSibling' : Hash(0x2b7ea66ec790) {.....................}, 
  'parentNode' : Hash(0x2b7ea66e9070) {...............................}, 
  'nodeType' : access, 
} */
/* 6 destructorDeclaration:sym:name ~SimpleQuote */
/* 7 sym:name delete_SimpleQuote */
/* 8 destructorHandler:sym:name SimpleQuote */
/* 9 feature:featurename 1 */
/* 10 destructorHandler:type  */
/* 11 destructorHandler:parms  */
/* 12 parentNode Hash(0x2b7ea66e9070) {
  'classtype' : QuantLib::SimpleQuote, 
  'name' : QuantLib::SimpleQuote, 
  'sym:symtab' : Hash(0x2b7ea66e89f0) {.......}, 
  'symtab' : Hash(0x2b7ea66e91d0) {.....}, 
  'nodeType' : class, 
  'allows_typedef' : 1, 
  'typepass:visit' : 1, 
  'allocate:visit' : 1, 
  'kind' : class, 
  'sym:name' : SimpleQuote, 
  'previousSibling' : Hash(0x2b7ea66e8dd0) {..............}, 
  'lastChild' : Hash(0x2b7ea66ec790) {.....................}, 
  'firstChild' : Hash(0x2b7ea66e9170) {....}, 
  'feature:featurename' : 1, 
  'allocate:default_constructor' : 1, 
  'allocate:has_constructor' : 1, 
  'allocate:public_constructor' : 1, 
  'allocate:copy_constructor' : 1, 
  'allocate:default_destructor' : 1, 
  'has_destructor' : 1, 
  'allocate:destructor' : 1, 
  'sym:cleanconstructor' : 1, 
  'has_constructor' : 1, 
  'parentNode' : Hash(0x2b7ea66e8a90) {............}, 
  'classtypeobj' : QuantLib::SimpleQuote, 
  'privatebaselist' : List[  ], 
  'protectedbaselist' : List[  ], 
  'baselist' : List[ Quote ], 
  'module' : Hash(0x2b7ea66e86b0) {....}, 
  'sym:overname' : __SWIG_0, 
  'typescope' : Hash(0x2b7ea66ea510) {.....}, 
} */
/* 13 destructorDeclaration:name ~SimpleQuote */
/* 14 decl f(). */
/* 15 access public */
/* 16 parms Hash(0x2b7ea66ed350) {
  'name' : self, 
  'wrap:disown' : 1, 
  'type' : p.QuantLib::SimpleQuote, 
  'self' : 1, 
  'hidden' : 1, 
  'nodeType' : parm, 
} */
/* 17 wrap:action delete arg1; */
/* 18 type void */
/* 19 sym:overname __SWIG_0 */
/* 20 destructorHandler:name ~SimpleQuote */
/* 0 name self */
/* 1 wrap:disown 1 */
/* 2 type p.QuantLib::SimpleQuote */
/* 3 self 1 */
/* 4 hidden 1 */
/* 5 nodeType parm */
//*************
namespace QuantLibAddin {
  class SimpleQuote : 
  public ObjectHandler::LibraryObject<QuantLib::SimpleQuote> {
  public:
    SimpleQuote(
      const boost::shared_ptr<ObjectHandler::ValueObject>& properties
      , double value            ,bool permanent)
    :ObjectHandler::LibraryObject<QuantLib::SimpleQuote>(properties, permanent) {
      libraryObject_ = boost::shared_ptr<QuantLib::SimpleQuote>(new QuantLib::SimpleQuote(value));
    }
  };
  
  namespace ValueObjects {
    class qlSimpleQuote : public ObjectHandler::ValueObject {
      friend class boost::serialization::access;
    public:
      qlSimpleQuote() {
        
      }
      qlSimpleQuote(
        const std::string& ObjectId,
        bool Permanent);
      
      const std::set<std::string>& getSystemPropertyNames() const;
      std::vector<std::string> getPropertyNamesVector() const;
      ObjectHandler::property_t getSystemProperty(const std::string&) const;
      void setSystemProperty(const std::string& name, const ObjectHandler::property_t& value);
      
    protected:
      static const char* mPropertyNames[];
      static std::set<std::string> mSystemPropertyNames;
      bool Permanent_;
      
      template<class Archive>
      void serialize(Archive& ar, const unsigned int) {
        boost::serialization::void_cast_register<qlSimpleQuote, ObjectHandler::ValueObject>(this, this);
        ar  & boost::serialization::make_nvp("ObjectId", objectId_)
        & boost::serialization::make_nvp("Permanent", Permanent_)
        & boost::serialization::make_nvp("UserProperties", userProperties);
      }
    };
    
    const char* qlSimpleQuote::mPropertyNames[] = {
      "Permanent"
    };
    
    std::set<std::string> qlSimpleQuote::mSystemPropertyNames(
      mPropertyNames, mPropertyNames + sizeof(mPropertyNames) / sizeof(const char*));
    
    const std::set<std::string>& qlSimpleQuote::getSystemPropertyNames() const {
      return mSystemPropertyNames;
    }
    
    std::vector<std::string> qlSimpleQuote::getPropertyNamesVector() const {
      std::vector<std::string> ret(
        mPropertyNames, mPropertyNames + sizeof(mPropertyNames) / sizeof(const char*));
      for (std::map<std::string, ObjectHandler::property_t>::const_iterator i = userProperties.begin();
        i != userProperties.end(); ++i)
      ret.push_back(i->first);
      return ret;
    }
    
    ObjectHandler::property_t qlSimpleQuote::getSystemProperty(const std::string& name) const {
      std::string nameUpper = boost::algorithm::to_upper_copy(name);
      if(strcmp(nameUpper.c_str(), "OBJECTID")==0)
      return objectId_;
      else if(strcmp(nameUpper.c_str(), "CLASSNAME")==0)
      return className_;
      else if(strcmp(nameUpper.c_str(), "PERMANENT")==0)
      return Permanent_;
      else
      OH_FAIL("Error: attempt to retrieve non-existent Property: '" + name + "'");
    }
    
    void qlSimpleQuote::setSystemProperty(const std::string& name, const ObjectHandler::property_t& value) {
      std::string nameUpper = boost::algorithm::to_upper_copy(name);
      if(strcmp(nameUpper.c_str(), "OBJECTID")==0)
      objectId_ = boost::get<std::string>(value);
      else if(strcmp(nameUpper.c_str(), "CLASSNAME")==0)
      className_ = boost::get<std::string>(value);
      else if(strcmp(nameUpper.c_str(), "PERMANENT")==0)
      Permanent_ = ObjectHandler::convert2<bool>(value);
      else
      OH_FAIL("Error: attempt to set non-existent Property: '" + name + "'");
    }
    
    qlSimpleQuote::qlSimpleQuote(
      const std::string& ObjectId,
    bool Permanent) :
    ObjectHandler::ValueObject(ObjectId, "qlSimpleQuote", Permanent),
    Permanent_(Permanent) {
      
    }
  }
}


//****FUNC*****
bool QuantLibAddin::close(double x, double y) {
  return QuantLib::close(x, y);
}
//****CTOR*****
std::string QuantLibAddin::qlSimpleQuote(const std::string &objectID, double value) {
  boost::shared_ptr<ObjectHandler::ValueObject> valueObject(
    new QuantLibAddin::ValueObjects::qlSimpleQuote(
      objectID, false));
  boost::shared_ptr<ObjectHandler::Object> object(
    new QuantLibAddin::SimpleQuote(
      valueObject, value, false));
  std::string returnValue =
  ObjectHandler::Repository::instance().storeObject(
    objectID, object, false, valueObject);
  return returnValue;
}
//****MEMB*****
double QuantLibAddin::qlSimpleQuotevalue(const std::string &objectID) {
  OH_GET_REFERENCE(x, objectID, QuantLibAddin::SimpleQuote, QuantLib::SimpleQuote);
  return x->value();
}


static ObjectHandler::Repository repository;
//f_init name=QuantLib::close
//f_init name=SimpleQuote
//f_init name=value
//f_init name=~SimpleQuote

