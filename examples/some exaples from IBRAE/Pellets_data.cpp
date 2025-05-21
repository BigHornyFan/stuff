#include "../Pellet/Pellets_data.h"

using namespace std;

void Pellets_Data::add_pellet(double z0, double z1, double r0, double r1, std::string& mat) {
	++amount_of_pellets;

	auto new_pellet = make_shared<Pellet>(z0, z1, r0, r1, mat);
	
	if (!pellets.empty()) {
		for (size_t i = 0; i < pellets.size(); i++) {
			if (pellets[i]->z0 > z0) {
				pellets.insert(pellets.begin() + i, new_pellet);
				return;
			}
		}
	}
	else {
		pellets.push_back(new_pellet);
		return;
	}
	
	pellets.push_back(new_pellet);
}

std::shared_ptr<Pellet> Pellets_Data::find_pellet(double z_coord) {
    std::shared_ptr<Pellet> result = nullptr;

    for (auto pel : pellets) {
        if (LESS_OR_EQ(pel->z0, z_coord) && LESS_OR_EQ(z_coord, pel->z1)) {
            result = pel;
            break;
        }
    }

    return result;
}

std::shared_ptr<Pellet> Pellets_Data::find_pellet(size_t number) {
	std::shared_ptr<Pellet> result = nullptr;

    if(number - 1 < pellets.size()) {
		result = pellets[number - 1];
    }

    return result;
}

double Pellet::get_lower_bound_of_pellet()
{
	return z0;
}

double Pellet::get_upper_bound_of_pellet()
{
	return z1;
}

double Pellet::get_internal_bound_of_pellet()
{
	return r0;
}

double Pellet::get_external_bound_of_pellet()
{
	return r1;
}
