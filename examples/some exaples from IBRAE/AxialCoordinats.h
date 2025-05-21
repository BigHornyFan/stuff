#pragma once
#include "../Utils/utils.h"
#include "../Cell/CylindricalMaterialCell.h"

class TransferBlock
{ // класс блока для переноса из входного файла
public:
    TransferBlock(std::string& input_name, double time, double stop, double in, double out, double up, double down) :
        name(input_name),
        start_time(time),
        z_stop(stop),
        r_in(in),
        r_ex(out),
        z_up(up),
        z_down(down),
        dynamic_mesh_flag(false){}

    TransferBlock(std::string& input_name, bool mesh_flag) :
        name(input_name),
        dynamic_mesh_flag(mesh_flag),
        start_time(0.0),
        z_stop(UNDEF_DOUBLE),
        r_in(0.0),
        r_ex(0.0),
        z_up(0.0),
        z_down(0.0)
    {}

    std::string name;

    double start_time;
    double z_stop;

    double r_in;
    double r_ex;
    double z_up;
    double z_down;
     // флаг определяет каким образом будем выбирать ячейки для перемещения
     // если off - то выбираем по границам из входного файла(по умолчанию)
     // если on - будем забирать по критерию(пока критерий: материал в слое единственный и он в пленке!)
     // Это нужно чтобы промоделировать всплытие материла
    bool dynamic_mesh_flag;
};

class InputAxialManager
{
public:
    InputAxialManager(std::vector<std::shared_ptr<TransferBlock>> blocks) : transfer_blocks(std::move(blocks)) {}
    InputAxialManager() = default;
    std::vector<std::shared_ptr<TransferBlock>> transfer_blocks;
};

class WorkTransferBlock
{ // рабочий класс блока для переноса
public:
    WorkTransferBlock(std::shared_ptr<TransferBlock>& input_block);

    bool calc_calculation_flag(double time);

    void write_restart(tRestart *rst) const;
    void read_restart(tRestart *rst, const shared_ptr<MaterialsCollection> &library);

    void new_to_old();
    void old_to_new();

    void solve_eqn(double dt,  double coolant_dens);

    double z_up_old;
    double z_down_old;

    double velocity;
    double z_up;
    double z_down;

    double mass;
    double dens;
    double volume;

    double time_to_start; // время начала переноса
    double z_stop; // координата остановки переноса
    double z_elem_up; // верхняя граница теплового элемента
    double z_elem_down; // нижняя граница теплового элемента

    bool stop_calculation; // время отключения модели, отключаем когда достигли нижней координаты
    bool send_from_material_mesh;// флаг для передачи из material_mesh в transfer_mesh
    bool delete_material_mesh;// флаг для удаления переносимых ячеек из material_mesh
    bool send_to_material_mesh;// флаг для возвращения материала из transfer_mesh в material_mesh
     
     // флаг определяет каким образом будем выбирать ячейки для перемещения
     // если off - то выбираем по границам из входного файла(по умолчанию)
     // если on - будем забирать по критерию(пока критерий: материал в слое единственный и он в пленке!)
     // Это нужно чтобы промоделировать всплытие материла
    bool dynamic_mesh_flag;

    std::shared_ptr<TransferBlock> transfer_block;
    std::vector<std::shared_ptr<CylindricalMaterialCell>> transfer_mesh; // основная сетка переноса
    std::vector<std::shared_ptr<CylindricalMaterialCell>> transfer_mesh_old; // сетка переноса со старого слоя
};

class AxialTransferManager
{
    public:
    AxialTransferManager(std::vector<std::shared_ptr<TransferBlock>>& input_blocks, size_t axial_size);

    void write_restart(tRestart *rst) const;
    void read_restart(tRestart *rst, const shared_ptr<MaterialsCollection> &library);

    void new_to_old();
    void old_to_new();

    void update(double dt, std::shared_ptr<WorkTransferBlock>& current_block);

    std::vector<std::shared_ptr<WorkTransferBlock>> work_transfer_blocks; // блоки переноса, рабочие
    std::vector<double> coolant_density; // плотность теплоносителя
};