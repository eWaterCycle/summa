#include "bmi_grpc_server.h"
#include "summa_bmi.h"

int main(int argc, char* argv[])
{
    Bmi* model = new SummaBmi();
    run_bmi_server(model, argc, argv);
    delete model;
    return 0;
}
