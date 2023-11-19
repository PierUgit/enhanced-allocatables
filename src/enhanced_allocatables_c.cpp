#include <cstdlib>
#include <cstdio>
#include <unordered_map>
#include <ISO_Fortran_binding.h>

#define MAX(x,y) (x > y ? x : y)
#define MIN(x,y) (x < y ? x : y)

using namespace std;

// map to store the capacity, indexed by the address of the first element
unordered_map<void*,int> capamap;

// returns the capacity    
extern "C"
int ea_capacity(CFI_cdesc_t* x) {
    if (capamap.find(x->base_addr) == capamap.end()) {
        printf("ea_capacity() error: base address %p not mapped",x->base_addr);
        exit(100);
    }
    return capamap[x->base_addr];
}

// prints some informations
extern "C"
void ea_printInfo(CFI_cdesc_t*  x) {
    printf("lbound = %d\n",x->dim[0].lower_bound);
    printf("size = %d\n",x->dim[0].extent);
    printf("address = %p %p\n",x->base_addr,NULL);
}

// "allocates" the array; actually Fortran allocate() must have been called before
extern "C"
void ea_alloc(CFI_cdesc_t*  x,
              int*          c) {
    int cc = MAX(x->dim[0].extent,1);
    cc = (c == NULL ? cc : MAX(cc,*c));
    free(x->base_addr);
    x->base_addr = malloc(cc * sizeof(float));
    capamap.insert({x->base_addr,cc});
}

// sets a new lower bound
extern "C"
void ea_setLBound(CFI_cdesc_t*  x,
                int*          lb) {
    x->dim[0].lower_bound = *lb;
}

// sets a new size; the capacity is supposed to be enough; the size cannot be zero
extern "C"
void ea_setSize(CFI_cdesc_t*  x,
              int*          newsize) {
    x->dim[0].extent = *newsize;
}

// sets a new capacity; free/malloc occur
extern "C"
void ea_setCapacity(CFI_cdesc_t*  x,
                    int*          newcap,
                    bool*         keep) {    
    float* tmp = (float*)malloc(*newcap * sizeof(float));
    if (keep) {
        int n = MIN(*newcap,ea_capacity(x));
        n = MIN(n,x->dim[0].extent);
        for (int i=0; i<n; i++) tmp[i] = ((float*)x->base_addr)[i];
    }
    capamap.erase(capamap.find(x->base_addr));
    free(x->base_addr);
    x->base_addr = tmp;
    capamap.insert({x->base_addr,*newcap});
}

// "deallocates" the array; Fortran deallocate() must be called after
extern "C"
void ea_deAlloc(CFI_cdesc_t*  x) {
    capamap.erase(capamap.find(x->base_addr));
    free(x->base_addr);
    x->base_addr = malloc(x->dim[0].extent * sizeof(float));
}
