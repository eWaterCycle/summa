#include "summa_bmi.h"
#include <stdio.h>
#include <stdlib.h>

int main()
{
  printf("Running the summa BMI C layer test");
  Bmi* model = new SummaBmi();
  model->initialize("\0");
  int ovar_count = 0;
  model->get_output_var_name_count(&ovar_count);
  printf("The number of output variables is %1d\n", ovar_count);
  char** ovars = (char**)malloc(ovar_count * sizeof(char*));
  for(int j = 0; j < ovar_count; ++j)
    {
      ovars[j] = (char*)malloc(BMI_MAX_VAR_NAME);
    }
  model->get_output_var_names(ovars);
  for(int j = 0; j < ovar_count; ++j)
    {
      char* units = (char*)malloc(BMI_MAX_VAR_NAME);
      model->get_var_units(ovars[j], units);
      printf("The output variable name is %s with units %s\n", ovars[j], units);
    }

  model->finalize(); //--> segfaults
  // delete model;
  for(int j = 0; j < ovar_count; ++j)
    {
      free(ovars[j]);
    }
  free(ovars);
}
