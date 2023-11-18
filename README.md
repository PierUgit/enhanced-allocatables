# Enhanced allocatables
Extending the allocatable arrays to be dynamically reallocatable/resizable
 
                          FOR DEMONSTRATION PURPOSE ONLY

Limitations
- default real kind only
- rank-1 only at the moment
- size limited to default integer values
- size=0 not yet managed
- not thread-safe
- The reallocatable/resizable features are achieved through directly manipulating the C array descriptors in the framework of the C interoperability: this is non standard, and therefore not guaranteed to be portable ! 

                          FOR DEMONSTRATION PURPOSE ONLY
