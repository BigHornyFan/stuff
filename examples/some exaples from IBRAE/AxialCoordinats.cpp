#pragma once
#include "AxialCoordinats.h"

WorkTransferBlock::WorkTransferBlock(std::shared_ptr<TransferBlock>& input_block) :
    transfer_block(input_block),
    stop_calculation(false),
    send_from_material_mesh(false),
    delete_material_mesh(true),
    send_to_material_mesh(false),
    dynamic_mesh_flag(input_block->dynamic_mesh_flag)
{
    velocity = mass = volume = dens = 0.0;
    z_elem_down = z_elem_up = 0.0;
    time_to_start = input_block->start_time;
    z_stop = input_block->z_stop;
    z_up_old = z_up = input_block->z_up;
    z_down_old = z_down = input_block->z_down;
}

bool WorkTransferBlock::calc_calculation_flag(double time) {
    // если текущее время расчета больше времени для начала падения
    // stop_calculation используем для отключения, не завист от времени
    // выставляем его, когда упадем до нижней координаты
    return LESS_OR_EQ(time_to_start, time) && !stop_calculation;
}

void WorkTransferBlock::solve_eqn(double dt,  double coolant_dens) {
    if(IS_ZERO(mass)) {
        return;
    }

    velocity += GRAVITY * volume * (dens - coolant_dens) / mass * dt;
    z_down -= velocity * dt;
    z_up -= velocity * dt;


    // на этом шаге достигнем нижней координаты
    bool is_stop = false;
    if(velocity > 0.0) {
        bool down_coord = LESS_OR_EQ(z_elem_down, z_down_old) && LESS_OR_EQ(z_down, z_elem_down);
        bool stop_coord = false;
        if (z_stop != UNDEF_DOUBLE) {
            stop_coord = LESS_OR_EQ(z_stop, z_down_old) && LESS_OR_EQ(z_down, z_stop);
        }
        is_stop = down_coord || stop_coord;
    }
    else if(velocity < 0.0) {
        bool up_coord = LESS_OR_EQ(z_down_old, z_elem_up) && LESS_OR_EQ(z_elem_up, z_down);
        bool stop_coord = false;
        if (z_stop != UNDEF_DOUBLE) {
            stop_coord = LESS_OR_EQ(z_down_old, z_stop) && LESS_OR_EQ(z_stop, z_down);
        }
        is_stop = up_coord || stop_coord;
    }
        
    if (is_stop) {
        if (z_stop == UNDEF_DOUBLE) {
            if(velocity > 0.0) {
               z_stop =  z_elem_down;
            }
            else {
               z_stop =  z_elem_up;  
            }
        }

        velocity = (z_down_old - z_stop) / dt;
        z_down = z_down_old - velocity * dt;
        z_up = z_up_old - velocity * dt;
        stop_calculation = true;
        send_to_material_mesh = true;
    }

    if (!transfer_mesh.empty()) {
        for (auto& cell : transfer_mesh) {
            double new_down = cell->down() - velocity * dt;
            double new_up = cell->up() - velocity * dt;
            if (velocity < 0.0) {
                cell->set_boundary(new_up, Direction::UP);
                cell->set_boundary(new_down, Direction::DOWN);
            }
            else {
                cell->set_boundary(new_down, Direction::DOWN);
                cell->set_boundary(new_up, Direction::UP);
            }
        }
    }

    if(is_stop) {
        velocity = 0.0;
    }
}

void WorkTransferBlock::write_restart(tRestart* rst) const {
    rst->Write(&z_down_old);
    rst->Write(&z_up_old);
    rst->Write(&z_down);
    rst->Write(&z_up);
    rst->Write(&z_elem_up);
    rst->Write(&z_elem_down);
    rst->Write(&velocity);
    rst->Write(&stop_calculation);
    rst->Write(&send_from_material_mesh);
    rst->Write(&delete_material_mesh);
    rst->Write(&send_to_material_mesh);
    rst->Write(&dynamic_mesh_flag);
    rst->Write(&mass);
    rst->Write(&volume);
    rst->Write(&dens);


    size_t tranv_size = transfer_mesh.size();
    rst->Write(&tranv_size);
    for (auto& cell : transfer_mesh) {
        cell->write_restart(rst);
    }

    tranv_size = transfer_mesh_old.size();
    rst->Write(&tranv_size);
    for (auto& cell : transfer_mesh_old) {
        cell->write_restart(rst);
    }
}

void WorkTransferBlock::read_restart(tRestart* rst, const shared_ptr<MaterialsCollection> &library) {
    rst->Read(&z_down_old);
    rst->Read(&z_up_old);
    rst->Read(&z_down);
    rst->Read(&z_up);
    rst->Read(&z_elem_up);
    rst->Read(&z_elem_down);
    rst->Read(&velocity);
    rst->Read(&stop_calculation);
    rst->Read(&send_from_material_mesh);
    rst->Read(&delete_material_mesh);
    rst->Read(&send_to_material_mesh);
    rst->Read(&dynamic_mesh_flag);
    rst->Read(&mass);
    rst->Read(&volume);
    rst->Read(&dens);

    size_t tranv_size = 0;
    rst->Read(&tranv_size);
    transfer_mesh.reserve(tranv_size);
    for (size_t i = 0; i < tranv_size; i++) {
        auto cell = make_shared<CylindricalMaterialCell>();
        cell->read_restart(rst, library);
        transfer_mesh.push_back(cell);
    }

    rst->Read(&tranv_size);
    transfer_mesh_old.reserve(tranv_size);
    for (size_t i = 0; i < tranv_size; i++) {
        auto cell = make_shared<CylindricalMaterialCell>();
        cell->read_restart(rst, library);
        transfer_mesh_old.push_back(cell);
    }
}

void WorkTransferBlock::new_to_old() {
    z_down_old = z_down;
    z_up_old = z_up;

    transfer_mesh_old.clear();
    transfer_mesh_old.reserve(transfer_mesh.size());
    for(auto& cell: transfer_mesh) {
        auto new_cell = make_shared<CylindricalMaterialCell>(cell);
        transfer_mesh_old.push_back(new_cell);
    }
}

void WorkTransferBlock::old_to_new() {
    z_down = z_down_old;
    z_up = z_up_old;

    transfer_mesh.clear();
    transfer_mesh.reserve(transfer_mesh_old.size());
    for(auto& cell: transfer_mesh_old) {
        auto new_cell = make_shared<CylindricalMaterialCell>(cell);
        transfer_mesh.push_back(new_cell);
    }
}


AxialTransferManager::AxialTransferManager(std::vector<std::shared_ptr<TransferBlock>>& input_blocks, size_t axial_size) {
    for (auto& block : input_blocks) {
        work_transfer_blocks.push_back(std::make_shared<WorkTransferBlock>(block));
    }

    coolant_density.resize(axial_size, 0.0);
}

void AxialTransferManager::update(double dt, std::shared_ptr<WorkTransferBlock>& current_block) {
    double avr_coolant_dens = 0.0;
    size_t num = 0;
    for(size_t t = 0; t < coolant_density.size(); t++) {
        if(!IS_ZERO(coolant_density[t])) {
            avr_coolant_dens +=coolant_density[t];
            ++num;
        }
    }

    if (num != 0) {
        avr_coolant_dens /= num;
    }

    current_block->solve_eqn(dt, avr_coolant_dens);
}

void AxialTransferManager::write_restart(tRestart* rst) const {
    for (auto& block : work_transfer_blocks) {
        block->write_restart(rst);
    }
}

void AxialTransferManager::read_restart(tRestart* rst, const shared_ptr<MaterialsCollection>& library) {
    for (auto& block : work_transfer_blocks) {
        block->read_restart(rst, library);
    }
}

void AxialTransferManager::new_to_old() {
    for (auto& block : work_transfer_blocks) {
        block->new_to_old();
    }
}

void AxialTransferManager::old_to_new() {
    for (auto& block : work_transfer_blocks) {
        block->old_to_new();
    }
}
