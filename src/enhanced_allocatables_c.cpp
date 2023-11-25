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

// "allocates" the array; actually Fortran allocate() must have been called before
extern "C"
void ea_alloc(CFI_cdesc_t*  x,
              int*          c) {
    int cc=1;
    for (int r=0; r < x->rank; r++) cc *= x->dim[r].extent;
    cc = MAX(cc,1);
    cc = (c == NULL ? cc : MAX(cc,*c));
    free(x->base_addr);
    x->base_addr = malloc(cc * sizeof(float));
    capamap.insert({x->base_addr,cc});
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
    void* tmp = malloc(*newcap * sizeof(float));
    if (keep) {
        int n = MIN(*newcap,ea_capacity(x));
        int size = 1;
        for (int r=0; r < x->rank; r++) size *= x->dim[r].extent;
        size = MAX(size,1);
        n = MIN(n,size);
        memcpy(tmp,x->base_addr,n*sizeof(float));
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
    //free(x->base_addr);
    //x->base_addr = malloc(x->dim[0].extent * sizeof(float));
}
