This is an example of visualization using Mayavi2 and QuantLib
python.  These examples are all distributed under the BSD License.

There are three components:

* The option.*.py files create the front end.
* The plotspace.py file interfaces does coordinate transformations
between plotting space and world space
* The pricing modules are using to get out the prices

Note that all of the QuantLib dependences are in the pricing modules.
This design is intentional as it allows one to swap out the QuantLib
pricing library and replace with another pricing library with a python
interface which contains proprietary code.
