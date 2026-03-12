#ifndef READER_VTK_H
#define READER_VTK_H

#include <vtkSmartPointer.h>
#include <vtkUnstructuredGrid.h>
#include <vtkUnstructuredGridReader.h>

class VtkReader
{
public:
    VtkReader();
    int initialize(const char *filename);
    void getHeaderInfo(char *format, int format_len, char *dataset, int dataset_len);
    int getNumPoints();
    void getPoints(double *x_arr, double *y_arr, double *z_arr);
    int getNumCells();
    long long getTotalConnectivitySize();
    void getCellInfo(long long *connectivity, long long *offsets, int *types);
    void getCellDataInt32(const char *dataName, int *data);
    void getCellDataFloat64(const char *dataName, double *data);
    void getPointDataInt32(const char *dataName, int *data);
    void getPointDataFloat64(const char *dataName, double *data);
    int getNumberOfPointDataComponents(const char *dataName);
    int getNumberOfCellDataComponents(const char *dataName);

private:
    vtkSmartPointer<vtkUnstructuredGridReader> reader;
    vtkSmartPointer<vtkUnstructuredGrid> grid;
    bool initialized;
};

#endif // READER_VTK_H