#include <cstring>
#include <libgen.h>
#include "summa_bmi.h"


extern "C" int init_summa(char*);
extern "C" int update_summa();
extern "C" int update_summa_until();
extern "C" int finalize_summa();
extern "C" int get_num_basins();
extern "C" int get_latlons(float*, float*);
extern "C" float get_summa_start_time();
extern "C" float get_summa_current_time();
extern "C" float get_summa_end_time();
extern "C" float get_summa_time_step();
extern "C" int get_num_ovars();
extern "C" void get_ovar_name(int*, char*);
extern "C" void get_ovar_units(int*, char*);
extern "C" void get_ovar_values(int*, float*);

SummaBmi::SummaBmi(){}

SummaBmi::~SummaBmi(){}

int SummaBmi::initialize(const char *config_file)
{
    int mem = 0;
    const char* dir = dirname(const_cast<char*>(config_file));
    init_summa(const_cast<char*>(dir));
    return BMI_SUCCESS;
}

int SummaBmi::update()
{
    int status = update_summa();
    if(status == 0)
    {
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::update_until(double time)
{
  int status = update_summa_until();
  if(status == 0)
    {
      return BMI_SUCCESS;
    }
  return BMI_FAILURE;
}

int SummaBmi::update_frac(double time)
{
    return BMI_FAILURE;
}

int SummaBmi::run_model()
{
    return BMI_FAILURE;
}

int SummaBmi::finalize()
{
    finalize_summa();
    return BMI_SUCCESS;
}

int SummaBmi::get_component_name(char* name) const
{
    strcpy(name, "summa");
    return BMI_SUCCESS;
}

int SummaBmi::get_input_var_name_count(int* count) const
{
    *count = 0;
    return BMI_SUCCESS;
}

int SummaBmi::get_output_var_name_count(int* count) const
{
    *count = get_num_ovars();
    return BMI_SUCCESS;
}

int SummaBmi::get_input_var_names(char **names) const
{
    return BMI_SUCCESS;
}

int SummaBmi::get_output_var_names(char **names) const
{
    int m = get_num_ovars();
    int k = 0;
    for(int i = 0; i < m; ++i)
    {
        k = i + 1;
        get_ovar_name(&k, names[i]);
    }
    return BMI_SUCCESS;
}

int get_ivar_index(const char* name)
{
    return -1;
}

int get_ovar_index(const char* name)
{
    int m = get_num_ovars();
    int k = 0;
    char buf[BMI_MAX_VAR_NAME];
    for(int i = 0; i < m; ++i)
    {
        k = i + 1;
        get_ovar_name(&k, buf);
        if(strcmp(name, buf) == 0)
        {
            return k;
        }
    }
    return -1;
}

int SummaBmi::get_var_type(const char* name, char* otype) const
{
    if(get_ivar_index(name) != -1 or get_ovar_index(name) != -1)
    {
        strcpy(otype, "float");
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::get_var_grid(const char* name, int* gtype) const
{
    if(get_ivar_index(name) != -1 or get_ovar_index(name) != -1)
    {
        *gtype = 1;
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::get_var_itemsize(const char* name, int* dest) const
{
    if(get_ivar_index(name) != -1 or get_ovar_index(name) != -1)
    {
        *dest = get_num_basins();
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::get_var_units(const char* name, char* dest) const
{
    int k = get_ivar_index(name);
    if(k > 0)
    {
//        get_ivar_units(&k, dest);
        return BMI_SUCCESS;
    }
    k = get_ovar_index(name);
    if(k > 0)
    {
        get_ovar_units(&k, dest);
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::get_var_nbytes(const char* name, int* dest) const
{
    *dest = 0;
    int size = 0;
    if(this->get_var_itemsize(name, &size) == BMI_SUCCESS)
    {
        *dest = size * sizeof(double);
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::get_current_time(double* dest) const
{
    *dest = (double)get_summa_current_time();
    return BMI_SUCCESS;
}

int SummaBmi::get_start_time(double* dest) const
{
    *dest = 0.;
    return BMI_SUCCESS;
}

int SummaBmi::get_end_time(double* dest) const
{
    *dest = (double)get_summa_end_time();
    return BMI_SUCCESS;
}

int SummaBmi::get_time_units(char* dest) const
{
    strcpy(dest, "hours since blabla\0");
    return BMI_SUCCESS;
}

int SummaBmi::get_time_step(double* dest) const
{
    *dest = (double)get_summa_time_step();
    return BMI_SUCCESS;
}

int SummaBmi::get_value(const char* name, void* dest) const
{
    int k = get_ovar_index(name);
    if(k > 0)
    {
        get_ovar_values(&k, (float*)dest);
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::get_value_ptr(const char* name, void** dest)
{
    return BMI_FAILURE;
}

int SummaBmi::get_value_at_indices(const char* name, void* dest, const int* pts, int numpts) const
{
    int k = get_ovar_index(name);
    if(k > 0)
    {
        float* buf = (float*)malloc(get_num_basins()*sizeof(float));
        get_ovar_values(&k, buf);
        for(int i=0; i < numpts; ++i)
        {
            ((float*)dest)[i] = buf[pts[i]];
        }
        free(buf);
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::set_value(const char* name, const void* src)
{
    return BMI_FAILURE;
}

int SummaBmi::set_value_ptr(const char* name, void** src)
{
    return BMI_FAILURE;
}

int SummaBmi::set_value_at_indices(const char* name, const int* pts, int numpts, const void* src)
{
    return BMI_FAILURE;
}

int SummaBmi::get_grid_size(int id, int* dest) const
{
    if(id == 1)
    {
        *dest = get_num_basins();
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::get_grid_rank(int id, int* dest) const
{
    if(id == 1)
    {
        *dest = 1;
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::get_grid_type(int id, char* dest) const
{
    if(id == 1)
    {
        strcpy(dest, "unstructured\n");
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::get_grid_shape(int id, int* dest) const
{
    if(id == 1)
    {
        *dest = get_num_basins();
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::get_grid_spacing(int id, double* dest) const
{
    return BMI_FAILURE;
}

int SummaBmi::get_grid_origin(int id, double* dest) const
{
    return BMI_FAILURE;
}

int SummaBmi::get_grid_x(int id, double* dest) const
{
    if(id == 1)
    {
        int n = get_num_basins();
        float* lats = (float*)malloc(sizeof(float)*n);
        float* lons = (float*)malloc(sizeof(float)*n);
        get_latlons(lats, lons);
        for(int i=0; i<n ; ++i)
        {
            dest[i] = (double)lons[i];
        }
        free(lats);
        free(lons);
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::get_grid_y(int id, double* dest) const
{
    if(id == 1)
    {
        int n = get_num_basins();
        float* lats = (float*)malloc(sizeof(float)*n);
        float* lons = (float*)malloc(sizeof(float)*n);
        get_latlons(lats, lons);
        for(int i=0; i<n ; ++i)
        {
            dest[i] = (double)lats[i];
        }
        free(lats);
        free(lons);
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::get_grid_z(int id, double* dest) const
{
    if(id == 1)
    {
        *dest = 0.;
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::get_grid_cell_count(int id, int* dest) const
{
    if(id == 1)
    {
        *dest = get_num_basins();
        return BMI_SUCCESS;
    }
    return BMI_FAILURE;
}

int SummaBmi::get_grid_point_count(int id, int* dest) const
{
    return BMI_FAILURE;
}

int SummaBmi::get_grid_vertex_count(int id, int* dest) const
{
    return BMI_FAILURE;
}

int SummaBmi::get_grid_connectivity(int id, int* dest) const
{
    return BMI_FAILURE;
}

int SummaBmi::get_grid_offset(int id, int* dest) const
{
    return BMI_FAILURE;
}
