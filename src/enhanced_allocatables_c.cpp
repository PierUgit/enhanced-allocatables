#include <cstdlib>
#include <cstdio>
#include <cstring>
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
    printf("rank = %d\n",x->rank);
    for (int r=0; r < x->rank; r++) {
        printf("lbound%d = %d\n",r+1,x->dim[r].lower_bound);
        printf("size%d = %d\n",r+1,x->dim[r].extent);
    }
    printf("address = %p %p\n",x->base_addr,NULL);
}

// "allocates" the array
extern "C"
void ea_alloc(CFI_cdesc_t*  x,
              int*          c) {
    CFI_index_t lb[2] = {1,1};
    CFI_index_t ub[2] = {*c,1};
    CFI_allocate(x,lb,ub,0);
    capamap.insert({x->base_addr,*c});
}

// sets a new lower bound
extern "C"
void ea_setLBound(CFI_cdesc_t*  x,
                  int*          r,
                  int*          lb) {
    x->dim[*r-1].lower_bound = *lb;
}

// sets a new size; the capacity is supposed to be enough; the size cannot be zero
extern "C"
void ea_setSize(CFI_cdesc_t*  x,
                int*          r,
                int*          newsize) {
    x->dim[*r-1].extent = *newsize;
}

// sets a new capacity; free/malloc occur
extern "C"
void ea_setCapacity(CFI_cdesc_t*  x,
                    int*          newcap,
                    bool*         keep) {
    if (*newcap == capamap[x->base_addr]) return;
    
    CFI_CDESC_T(1) y_;
    CFI_cdesc_t* y = (CFI_cdesc_t*)&y_;
    CFI_establish(y,NULL,CFI_attribute_allocatable,CFI_type_float,0,1,NULL);
    CFI_index_t lb[1] = {1};
    CFI_index_t ub[1] = {*newcap};
    CFI_allocate(y,lb,ub,0);
    if (*keep) {
        int n = 1;
        for (int r=0; r < x->rank; r++) n *= x->dim[r].extent;
        n = MIN(n,ea_capacity(x));
        n = MIN(n,*newcap);
        if (n > 0) memcpy(y->base_addr,x->base_addr,n*sizeof(float));
    }
    capamap.erase(capamap.find(x->base_addr));
    std::swap(x->base_addr,y->base_addr);
    capamap.insert({x->base_addr,*newcap});
    CFI_deallocate(y);
}

// "deallocates" the array; Fortran deallocate() must be called after
extern "C"
void ea_deAlloc(CFI_cdesc_t*  x) {
    capamap.erase(capamap.find(x->base_addr));
    CFI_deallocate(x);
}
