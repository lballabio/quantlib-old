#include <objectwidget.hpp>

ObjectWidget::ObjectWidget(const std::string &s, const int &i) {
   // construct widget object and point member variable at it
   boost::shared_ptr<Widget> temp(new Widget(s, i));
   widget_ = temp;
   // populate base class Property vector
   any_ptr anyString(new boost::any(widget_->s()));
   any_ptr anyInt(new boost::any(widget_->i()));
   ObjectProperty propString(PROPERTY_STR, anyString);
   ObjectProperty propInt(PROPERTY_INT, anyInt);
   properties_.push_back(propString);
   properties_.push_back(propInt);    
}
                                                                                
// wrapper for underlying member function
void ObjectWidget::update(const std::string &s, const int &i) {
   widget_->update(s, i);
   // update Property vector
   *properties_[IDX_STR]() = s;
   *properties_[IDX_INT]() = i;    
}

boost::shared_ptr<void> ObjectWidget::getReference() const {
   return boost::static_pointer_cast<void>(widget_);
}
