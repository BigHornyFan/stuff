#pragma once
#include <vector>
#include <memory>
#include <string>

#include "../Utils/utils.h"
#include "../Cell/DividableCell.h"

// класс таблетки, хранит данные по каждой таблетке 
class Pellet {
public:
    Pellet() = default;
    Pellet(double z0, double z1, double r0, double r1, std::string& mat) :
        z0{ z0 },
        z1{ z1 },
        r0{ r0 },
        r1{ r1 },
        material{ mat } {}

    // методы, возващающие границы таблетки
    double get_lower_bound_of_pellet();
    double get_upper_bound_of_pellet();
    double get_external_bound_of_pellet();
    double get_internal_bound_of_pellet();

    double z0;
    double z1;
    double r0;
    double r1;
    std::string material;
	bool is_liquid = false;
    std::vector<std::shared_ptr<DividableCell>> ptr_mat_cells; 
};

class Pellets_Data {

public:
	Pellets_Data() = default;
	
	// число таблеток
	int amount_of_pellets = 0;

	// вектор ссылок на каждую таблетку
	std::vector<std::shared_ptr<Pellet>> pellets;

	// добавляем таблетку, причем они кладутся в порядке от саомй нижней к самой выосокй
	void add_pellet(double z0, double z1, double r0, double r1, std::string& mat);

	// ищем таблетку по координате или номеру, возвращает ссылку на таблетку 
	std::shared_ptr<Pellet> find_pellet(double z_coord);
	std::shared_ptr<Pellet> find_pellet(size_t number);
};