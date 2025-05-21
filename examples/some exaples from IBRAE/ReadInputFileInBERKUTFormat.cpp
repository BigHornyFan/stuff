#include "InputFileReader.h"
#include "../Materials/str_switch.h"
#include "../Materials/DataBaseMaterial.h"

// для определения имени тега с данными твэльного модуля
extern std::string ModuleName;

bool InputFileReader::readRootInBERKUTFormat(const map<string,string> &fuelRodList) {
	// читает только основные параметры Беркута и тепловые структуры
	// граничные условия - всегда внешний тепловой поток (приходит из Беркута или Гидры)
	// граничные условия для плёнки - пока всегда "стенка"
	// трение плёнки с газом приходит из Гидры
	// т.о. блок с нодализацией тоже не нужен, однако требуется создать правильные связи (аналог метода createConnections)

	if (fuelRodList.empty()) return true;

	bool readOK = true;
	// 29.11.2023 Убрал считывание таблиц из беркута, так как обмен идёт через обменный модуль
	//if (!isActiveNeutronics) { // если нет нейтронного модуля, тепловыделение может задаваться в таблицах
	//	readOK = readMultiTables();
	//}

    std::string MainBerkutTag;
	if (ModuleName.find("SAFR") != std::string::npos) {
		MainBerkutTag = "MainBERKUT";   // для ЕВКЛИДа
	}

	if (   ModuleName.find("SACURA") != std::string::npos
		|| ModuleName.find("HEFEST_CORE") != std::string::npos) {
		MainBerkutTag = "MainFuelRods"; // для ТИТАНа
	}

	// считывание основных параметров Беркута (MainBERKUT)
	pugi::xml_node mainBERKUT = getRequiredChildInXmlNode(MainBerkutTag, root);
	
	// температура в стандартных условиях (при которых измеряются геометрические размеры твэлов):
	double tempSC = readRequiredDoubleAttributeFromXmlNode("TRREF0", mainBERKUT, "ERROR: Standard conditions temperature (TRREF0) is absent in MainBERKUT tag");
	// начальная температура:
	double tempInit = readRequiredDoubleAttributeFromXmlNode("TRSTART", mainBERKUT, "ERROR: Initial temperature (TRSTART) is absent in MainBERKUT tag");
	// материал оболочки по-умолчанию:
	string claddingMaterial = readRequiredStringAttributeFromXmlNode("ICLAD", mainBERKUT, "ERROR: Cladding material type (ICLAD) is absent in MainBERKUT tag");



	// относительная плотность топлива, равная отношению плотности изготовления к теоретической плотности материала:
	//double relDens = readRequiredDoubleAttributeFromXmlNode("RELDENS", mainBERKUT, "ERROR: Relative density (RELDENS) is absent in MainBERKUT tag");
	// массовая доля оксида или нитрида плутония:
	//double puFraction = readRequiredDoubleAttributeFromXmlNode("PUPART", mainBERKUT, "ERROR: Plutonium fraction (PUPART) is absent in MainBERKUT tag");
	// тип топлива по-умолчанию:
	//string fuelMaterial = readRequiredStringAttributeFromXmlNode("IFUEL", mainBERKUT, "ERROR: Fuel material type (IFUEL) is absent in MainBERKUT tag");



	// газ закачки:
	string gasName = "";
	string gasNameIGAS = readOptionalStringAttributeFromXmlNode("IGAS", mainBERKUT);
	string gasNameIGAP = readOptionalStringAttributeFromXmlNode("IGAP", mainBERKUT);
	if (gasNameIGAP.empty()) {
		if (gasNameIGAS.empty()) {
			throw std::invalid_argument("ERROR: Gas material type (IGAP or IGAS) is absent in MainBERKUT tag");
		}
		else {
			gasName = gasNameIGAS;
		}
	} else {
		gasName = gasNameIGAP;
	}


	// относительная плотность топлива, равная отношению плотности изготовления к теоретической плотности материала:
	double relDens;
	// массовая доля оксида или нитрида плутония:
	double puFraction;
	// тип топлива по-умолчанию:
	string fuelMaterial;



	string fuelNameIFUEL = readOptionalStringAttributeFromXmlNode("IFUEL", mainBERKUT);

    vector<shared_ptr<BerkutBatch>> input_batches;
    if (fuelNameIFUEL.empty()) {
        string fuelNameBatchPell = readOptionalStringAttributeFromXmlNode("BatchPell", mainBERKUT);
        if (fuelNameBatchPell.empty()) {
            throw std::invalid_argument("ERROR: Fuel material (IFUEL or BatchPell) is absent in MainBERKUT tag");
        }

        auto ParamBatchList = getRequiredChildInXmlNode("ParamBatch", mainBERKUT);
        for (auto batch = ParamBatchList.child("Batch"); batch; batch = batch.next_sibling("Batch")) {
            string batch_name = readRequiredStringAttributeFromXmlNode("Name", batch, "ERROR: No name of Batch");
            string fuel_name = readRequiredStringAttributeFromXmlNode("IFuel", batch, "ERROR: Batch fuel  material type (IFuel) is absent in Batch tag " + batch_name);
            double rel_dens = readOptionalDoubleAttributeFromXmlNode("RelDens", batch, 1.0);
            double pu_part = readOptionalDoubleAttributeFromXmlNode("PuPart", batch, 0.0);
            input_batches.push_back(std::make_shared<BerkutBatch>(batch_name, fuel_name, rel_dens, pu_part));

            if (batch_name == fuelNameBatchPell) {
                fuelMaterial = fuel_name;
                relDens = rel_dens;
                puFraction = pu_part;
            }
        }
    }
    else {
        fuelMaterial = fuelNameIFUEL;
        relDens = readRequiredDoubleAttributeFromXmlNode("RELDENS", mainBERKUT, "ERROR: Relative density (RELDENS) is absent in MainBERKUT tag");
        puFraction = readRequiredDoubleAttributeFromXmlNode("PUPART", mainBERKUT, "ERROR: Plutonium fraction (PUPART) is absent in MainBERKUT tag");
    }

	
	// давление газа закачки
	double gasPressure = readRequiredDoubleAttributeFromXmlNode("PGAP0", mainBERKUT, "ERROR: Gas pressure (PGAP0) is absent in MainBERKUT tag");
	// таблица для энерговыделения по-умолчанию:
	string tableHeatPower = readOptionalStringAttributeFromXmlNode("HeatPower", mainBERKUT);
	
	// считывание основных параметров теплового модуля (mdlHeat в MainBERKUT)
	auto mdlHeat = getRequiredChildInXmlNode("Modules", mainBERKUT);
	mdlHeat = getRequiredChildInXmlNode("mdlHeat", mdlHeat);
	
	// флаг включения модуля:
	bool moduleUse = isOptionalFlagSwitchedOnInXmlNode("Use", mdlHeat, true, "1", "0");

	bool is_mesh_type_uniRad = isOptionalFlagSwitchedOnInXmlNode("GridType", mdlHeat, true, "GRD_DIST", "GRD_VOL");
	typeRadialMesh meshType = (is_mesh_type_uniRad? uniRad : uniVol);
		
	size_t Nr_hole = UNDEF_SIZE; // число радиальных ячеек в центральном отверстии
	size_t Nr_pellet = UNDEF_SIZE; // число радиальных ячеек в топливной таблетке
	size_t Nr_gap = UNDEF_SIZE; // число радиальных ячеек в газовом зазоре
	size_t Nr_cladding = UNDEF_SIZE; // число радиальных ячеек в оболочке
	// считывание параметров радиальной сетки (RadLayCount в mdlHeat)
	readRadLayCountInBERKUTFormat(mdlHeat, Nr_hole, Nr_pellet, Nr_gap, Nr_cladding);


	BerkutParam param( tempSC, tempInit, relDens, puFraction, gasPressure, fuelMaterial, claddingMaterial, gasName
					 , tableHeatPower, moduleUse, meshType, Nr_hole, Nr_pellet, Nr_gap, Nr_cladding, input_batches);

    shared_ptr<MaterialsParam> mat_clad = make_shared<MaterialsParam>(claddingMaterial, 0.0, 0.0, 0.0, 0.0);
    shared_ptr<MaterialsParam> mat_gas = make_shared<MaterialsParam>(gasName, 0.0, 0.0, 0.0, gasPressure);

    vector<shared_ptr<MaterialsParam>> input_mats;
    if (input_batches.empty()) {
        shared_ptr<MaterialsParam> mat_fuel = make_shared<MaterialsParam>(fuelMaterial, relDens, puFraction, 0.0, 0.0);
        input_mats = { mat_fuel, mat_clad, mat_gas };
    }
    else {
        input_mats = { mat_clad, mat_gas };
        for (auto& batch : input_batches) {
            input_mats.push_back(make_shared<MaterialsParam>(batch->fuel_name, batch->rel_dens, batch->pu_part, 0.0, 0.0));
        }
    }

	check_materials(input_mats);

	readHeatStructsInBERKUTFormat(param, fuelRodList);

	return readOK;
}

bool InputFileReader::readRadLayCountInBERKUTFormat(const pugi::xml_node &mdl, size_t &Nr_hole, size_t &Nr_pellet, size_t &Nr_gap, size_t &Nr_cladding) {
	auto radLayCount = getRequiredChildInXmlNode("RadLayCount", mdl);
	
	enum { HOLE = 0, PELLET = 1, GAP = 2, CLADDDING = 3 };
	vector<pair<pugi::char_t*,size_t>> NrArray;
	NrArray.resize(4);
	NrArray[HOLE] = pair<pugi::char_t*,size_t>((char*)"cntHole",UNDEF_SIZE);
	NrArray[PELLET] = pair<pugi::char_t*,size_t>((char*)"cntPell",UNDEF_SIZE);
	NrArray[GAP] = pair<pugi::char_t*,size_t>((char*)"cntGap",UNDEF_SIZE);
	NrArray[CLADDDING] = pair<pugi::char_t*,size_t>((char*)"cntClad",UNDEF_SIZE);

	for (auto &Nr : NrArray) {
		Nr.second = readRequiredIntAttributeFromXmlNode(Nr.first, radLayCount, "ERROR: Radial cell number " + string(Nr.first) + " is absent in RadLayCount tag");
	}

	Nr_hole = NrArray[HOLE].second;
	Nr_pellet = NrArray[PELLET].second;
	Nr_gap = NrArray[GAP].second;
	Nr_cladding = NrArray[CLADDDING].second;

	if (Nr_gap > 1) {
		throw std::invalid_argument("ERROR: Cell number for gas gap (cntGap) must be unity by combined SAFR and BERKUT calculation");
	}

	return true;
}

bool InputFileReader::readHeatStructsInBERKUTFormat(const BerkutParam &param, const map<string, string> &fuelRodList) {
	auto h_bound_zero_heat_flux = make_shared<HeatBound>("Core_zero_heat_flux", HeatBound::TYPE_Q);
	h_bound_zero_heat_flux->set_table_heat_flux_flag(VariableType::Const);
	h_bound_zero_heat_flux->set_heat_flux(0.);
	heatBoundsInInputFile.push_back(h_bound_zero_heat_flux);

	for (const auto &rod_names : fuelRodList) {
		// только твэлы, пэлы не моделируются!
		// поиск соотвествующего твэла в Беркуте
		pugi::xml_node rod;
		string name; // короткое имя
		bool it_fuel_rod = false;
		for (rod = root.child("FuelRod"); rod; rod = rod.next_sibling("FuelRod")) {
			name = readRequiredStringAttributeFromXmlNode("Name", rod, "ERROR: No name of FuelRod");
			if (name == rod_names.second) {
				it_fuel_rod = true;
				break;
			}
		}

        if (!it_fuel_rod) {
            for (rod = root.child("AbsorbingRod"); rod; rod = rod.next_sibling("AbsorbingRod")) {
                name = readRequiredStringAttributeFromXmlNode("Name", rod, "ERROR: No name of AbsorbingRod");
                if (name == rod_names.second) {
                    break;
                }
            }
        }

		if (rod.empty()) {
			throw std::invalid_argument("ERROR: Discription of fuel rod: " + rod_names.second + " from FuelRodList is absent in BERKUT");
		}

		auto heatElement = std::dynamic_pointer_cast<HeatStruct>(find_object_by_name<HeatElement>(rod_names.first, inputHeatElements));
		if (!readRodMeshAndMaterial(rod, heatElement, param)) return false;

		if (!isActiveNeutronics) { // если нет нейтронного модуля, тепловыделение может задаваться в таблицах
			string tableName = readOptionalStringAttributeFromXmlNode("HeatPower", rod, param.tableHeatPower);
			if (!tableName.empty()) {
				heatElement->set_multi_table_energy_release_flag(true);
				// 29.11.2023 Убрал считывание таблиц из беркута, так как обмен идёт через обменный модуль
				//auto m_table = find_object_by_name_or_fail<MultiTable>(tableName, inputMultiTables);
				//heatElement->set_multi_table_energy_release(m_table);
			}
		}

		heatElement->set_sin(1.); // твэлы всегда вертикальные
		heatElement->set_heat_solve(false);

		auto fuelRod = inputCore->find_fuel_rod_by_name(heatElement->get_name());
		heatElement->set_film_solve(fuelRod->FilmSolve);
		heatElement->set_external_film_materials(fuelRod->external_film_materials);
		heatElement->set_internal_film_materials(fuelRod->internal_film_materials);
		heatElement->set_incomplete_melting_materials(fuelRod->incomplete_melting_materials);
		heatElement->set_incomplete_melting_flag(!fuelRod->incomplete_melting_materials.empty());
		heatElement->set_material_for_external_transfer(fuelRod->material_for_external_transfer);
		heatElement->set_plenum_gas_pressure(1e5); // to avoid value check, real value will be received from Berkut
		heatElement->set_merge_liquid_mat_cells_flag(fuelRod->MergeMultiMatCells);
		heatElement->RadiusExternalMax = fuelRod->radius_external_max;
		heatElement->set_heat_extension_for_liquid_cells(fuelRod->HeatExtensionForLiquidCells);
		heatElement->set_evaporation_flag(fuelRod->EvaporationSolve);
		heatElement->is_relative_grid = fuelRod->is_relative_grid;
		heatElement->set_eutectic_name(fuelRod->eutectic_name);

		//множители для коэфициента трения со стенкой
		heatElement->set_FrictionFactorInner(fuelRod->FrictionFactorInner);
		heatElement->set_FrictionFactorExternal(fuelRod->FrictionFactorExternal);
		// множитель для коэффициента теплоотдачи от плёнки
		heatElement->set_FilmHeatTransferCoefInner(fuelRod->FilmHeatTransferCoefInner);
	    heatElement->set_FilmHeatTransferCoefExternal(fuelRod->FilmHeatTransferCoefExternal);

	    // количество ячеек вдоль теплового элемента
		size_t cell_number = heatElement->calc_axial_number_by_section(); 
		if(heatElement->is_relative_grid) { // если используем относительну сетку, то число ячеек должно совпадать с каналом
		    cell_number = inputCore->channel_axial_size;
		}
		heatElement->set_axial_mesh_size(cell_number);
		std::vector lin_pow(cell_number, 0.);
		heatElement->set_linear_power(lin_pow);

		shared_ptr<HeatBound> h_bound_ex;
		if (isActiveHydraulics) { // теплообмен с теплоносителем
			h_bound_ex = make_shared<HeatBoundCell>("External_Channel_in_Core_" + heatElement->get_name(), HeatBound::TYPE_T_ENV_EX, cell_number);
			

			auto gb = make_shared<GasBound>("External_Channel_in_Core_" + heatElement->get_name(), GasBoundaryType::HYDRA_CELL);
			heatElement->setBoundary(gb, HeatElement::EXTERNAL);
		}

		else { // температура стенки из твэльного модуля
			h_bound_ex = make_shared<HeatBoundCell>("Temperature_Wall_in_Core_" + heatElement->get_name(), HeatBound::TYPE_TWALL_EX, cell_number);
		}
		heatBoundsInInputFile.push_back(h_bound_ex);

		heatElement->setBoundary(h_bound_zero_heat_flux, HeatElement::INTERNAL);
		heatElement->setBoundary(h_bound_ex, HeatElement::EXTERNAL);

        for (auto connection : connections) {
            if (connection->TypeFrom == "FuelRod" || connection->TypeFrom == "AbsorbingRod") {
                if (name == connection->NameFrom) {
                    if (connection->TypeTo == "FilmBound") {
                        unsigned int bound_type = UNDEF_SIZE;
                        if (connection->Boundary == "Upper") {
                            bound_type = HeatElement::UPPER;
                        }
                        else if (connection->Boundary == "Down") {
                            bound_type = HeatElement::DOWN;
                        }
                        else {
                            throw std::invalid_argument("ERROR: InputFileReader: Invalid boundary type: " + connection->Boundary);
                        }

                        auto film_bound = find_object_by_name_or_fail<FilmBound>(connection->NameTo, filmBounds);
                        // для расчёта твэлов в составе реактора, только такие гран.условия имеют смысл:
                        if (film_bound->get_type() != BoundaryType::WALL && film_bound->get_type() != BoundaryType::FREE) {
                            throw std::invalid_argument("ERROR: Invalid film boundary type for heat struct: " + heatElement->get_name());
                        }

                        heatElement->setBoundary(film_bound, bound_type);
                    }
                    else if (connection->TypeTo == "MeltPool") {
                        unsigned int bound_type = UNDEF_SIZE;
                        if (connection->Boundary == "Upper") {
                            bound_type = HeatElement::UPPER;
                        }
                        else if (connection->Boundary == "Down") {
                            bound_type = HeatElement::DOWN;
                        }
                        else {
                            throw std::invalid_argument("ERROR: InputFileReader: Invalid boundary type: " + connection->Boundary);
                        }

                        auto pool = find_object_by_name_or_fail<MeltPool>(connection->NameTo, meltPools);
                        if (bound_type == HeatElement::UPPER) {
                            pool->add_heat_struct_down(static_cast<std::shared_ptr<HeatElement>>(heatElement));
                        }
                        else if (bound_type == HeatElement::DOWN) {
                            auto elem = static_cast<std::shared_ptr<HeatElement>>(heatElement);
                            pool->add_pool_data(elem);
                        }
                        else {
                            throw std::invalid_argument("ERROR: InputFileReader::createConnections: Invalid boundary type: " + connection->Boundary);
                        }

                        auto film_bound = make_shared<FilmBound>("FilmBoundFree_" + pool->get_name(), BoundaryType::FREE);
                        heatElement->setBoundary(film_bound, bound_type);
                    }
                }
            }
        }

		// проверка, что есть оба граничных условия для плёнки:
		if (heatElement->isFilmBoundEmpty(HeatElement::UPPER) || heatElement->isFilmBoundEmpty(HeatElement::DOWN)) {
			throw std::invalid_argument("ERROR: Invalid connection: not all film boundary conditions are input for heat struct: " + heatElement->get_name());
		}
	}

	return true;
}

bool InputFileReader::readRodMeshAndMaterial(const pugi::xml_node &rod, const shared_ptr<HeatStruct> &heatElement, const BerkutParam &par) {
	vector<pair<string,pugi::xml_node>> meshSets;
	for (pugi::xml_node meshSet = rod.child("MeshSet"); meshSet; meshSet = meshSet.next_sibling("MeshSet")) {
		string name = readRequiredStringAttributeFromXmlNode("Name", meshSet, "ERROR: No name of MeshSet");

		if (   name != "TopVol"
			&& name != "TopFuel"
			&& name != "ActFuel"
			&& name != "BtmFuel"
			&& name != "BtmVol") {
				throw std::invalid_argument("ERROR: Incorrect name of MeshSet: " + name);
		}

		for (size_t i = 0; i < meshSets.size(); i++) {
			if (name == meshSets[i].first) {
				throw std::invalid_argument("ERROR: Repeated input of MeshSet's name: " + name);
			}
		}

		meshSets.push_back(pair<string,pugi::xml_node>(name,meshSet));
	}

	bool isTopVol = false;
	bool isTopFuel = false;
	bool isActFuel = false;
	bool isBtmFuel = false;
	bool isBtmVol = false;
	pugi::xml_node topVol;
	pugi::xml_node topFuel;
	pugi::xml_node actFuel;
	pugi::xml_node btmFuel;
	pugi::xml_node btmVol;
	for (size_t i = 0; i < meshSets.size(); i++) {
		if (meshSets[i].first == "TopVol") {
			isTopVol = true;
			topVol = meshSets[i].second;
		}

		if (meshSets[i].first == "TopFuel") {
			isTopFuel = true;
			topFuel = meshSets[i].second;
		}

		if (meshSets[i].first == "ActFuel") {
			isActFuel = true;
			actFuel = meshSets[i].second;
		}

		if (meshSets[i].first == "BtmFuel") {
			isBtmFuel = true;
			btmFuel = meshSets[i].second;
		}

		if (meshSets[i].first == "BtmVol") {
			isBtmVol = true;
			btmVol = meshSets[i].second;
		}
	}

	if (!(isTopFuel || isActFuel || isBtmFuel)) {
		throw std::invalid_argument("ERROR: No TopFuel or ActFuel or BtmFuel in FuelRod " + heatElement->get_name());
	}

	double z0 = 0.0;
	if (isBtmVol) {
		z0 = readAxialRodPart(btmVol, heatElement, par, z0);
	}
	if (isBtmFuel) {
		z0 = readAxialRodPart(btmFuel, heatElement, par, z0);
	}
	if (isActFuel) {
		z0 = readAxialRodPart(actFuel, heatElement, par, z0);
	}
	if (isTopFuel) {
		z0 = readAxialRodPart(topFuel, heatElement, par, z0);
	}
	if (isTopVol) {
		z0 = readAxialRodPart(topVol, heatElement, par, z0);
	}

	return true;
}

double InputFileReader::readAxialRodPart(const pugi::xml_node &rodPart, const shared_ptr<HeatStruct> &heatElement, const BerkutParam &main_param, const double &z) {
	BerkutParam param = main_param;

	// переопределение типа топлива:
	param.fuelMaterial = readOptionalStringAttributeFromXmlNode("IFUEL", rodPart, param.fuelMaterial);
	// переопределение матириала оболочки:
	param.claddingMaterial = readOptionalStringAttributeFromXmlNode("ICLAD", rodPart, param.claddingMaterial);
	// переопределение типа газового зазора:
	param.gasName = readOptionalStringAttributeFromXmlNode("IGAS", rodPart, param.gasName);

    shared_ptr<MaterialsParam> mat_clad = make_shared<MaterialsParam>(param.claddingMaterial, 0.0, 0.0, 0.0, 0.0);
    shared_ptr<MaterialsParam> mat_gas = make_shared<MaterialsParam>(param.gasName, 0.0, 0.0, 0.0, param.gasPressure);

    vector<shared_ptr<MaterialsParam>> input_mats;
    if (param.berkut_batches.empty()) {
		shared_ptr<MaterialsParam> mat_fuel = make_shared<MaterialsParam>(param.fuelMaterial, param.relDens, param.puFraction, 0.0, 0.0);
        input_mats = { mat_fuel, mat_clad, mat_gas };
    }
    else {
        input_mats = { mat_clad, mat_gas };
        for (auto& batch : param.berkut_batches) {
            input_mats.push_back(make_shared<MaterialsParam>(batch->fuel_name, batch->rel_dens, batch->pu_part, 0.0, 0.0));
        }
    }

	check_materials(input_mats);

	if (!rodPart.child("Modules").empty()) {
		pugi::xml_node mdlHeat = rodPart.child("Modules");
		if (!mdlHeat.child("mdlHeat").empty()) {
			mdlHeat = mdlHeat.child("mdlHeat");
			// считывание параметров радиальной сетки (RadLayCount в mdlHeat)
			readRadLayCountInBERKUTFormat(mdlHeat, param.Nr_hole, param.Nr_pellet, param.Nr_gap, param.Nr_cladding);
		}
	}

	vector<pugi::xml_node> meshes;
	for (pugi::xml_node mesh = rodPart.child("Mesh"); mesh; mesh = mesh.next_sibling("Mesh")) {
		meshes.push_back(mesh);
	}

	size_t cell_number = 0;
	double z0 = z;
	for (auto iMesh = meshes.rbegin(); iMesh != meshes.rend(); iMesh++) {
		auto mesh = *iMesh;
		// число одинаковых осевых ячеек:
		size_t Nz = readRequiredIntAttributeFromXmlNode("Repeat", mesh, "ERROR: Axial cell number (Repeat) is absent in Mesh tag");
		cell_number += Nz;

		BerkutParam mesh_param = param;
		// переопределение типа топлива:
		mesh_param.fuelMaterial = readOptionalStringAttributeFromXmlNode("IFUEL", mesh, mesh_param.fuelMaterial);
		// переопределение матириала оболочки:
		mesh_param.claddingMaterial = readOptionalStringAttributeFromXmlNode("ICLAD", mesh, mesh_param.claddingMaterial);
		// переопределение типа газового зазора:
		mesh_param.gasName = readOptionalStringAttributeFromXmlNode("IGAS", mesh, mesh_param.gasName);

        shared_ptr<MaterialsParam> mesh_mat_clad = make_shared<MaterialsParam>(mesh_param.claddingMaterial, 0.0, 0.0, 0.0, 0.0);
        shared_ptr<MaterialsParam> mesh_mat_gas = make_shared<MaterialsParam>(mesh_param.gasName, 0.0, 0.0, 0.0, param.gasPressure);

        vector<shared_ptr<MaterialsParam>> mesh_input_mats;
        if (param.berkut_batches.empty()) {
            shared_ptr<MaterialsParam> mesh_mat_fuel = make_shared<MaterialsParam>(mesh_param.fuelMaterial, param.relDens, param.puFraction, 0.0, 0.0);
            mesh_input_mats = { mesh_mat_fuel, mesh_mat_clad, mesh_mat_gas };
        }
        else {
            mesh_input_mats = { mesh_mat_clad, mesh_mat_gas };
            for (auto& batch : param.berkut_batches) {
                mesh_input_mats.push_back(make_shared<MaterialsParam>(batch->fuel_name, batch->rel_dens, batch->pu_part, 0.0, 0.0));
            }
        }
				
		check_materials(mesh_input_mats);
		
		pugi::xml_node matter = getRequiredChildInXmlNode("Matter", mesh, "ERROR: Matter tag is absent in Mesh tag");
		
		// высота осевой ячейки, м:
		double height = readRequiredDoubleAttributeFromXmlNode("Height", matter, "ERROR: No Height in Matter tag");
		// радиус центрального отверстия, м:
		double r_hole = 0.5 * readRequiredDoubleAttributeFromXmlNode("dmHole", matter, "ERROR: No dmHole in Matter tag");
		// внешний радиус таблетки, м:
		double r_pellet = 0.5 * readRequiredDoubleAttributeFromXmlNode("dmPellet", matter, "ERROR: No dmPellet in Matter tag");
		// внешний радиус газового зазора, м:
		double r_gap = 0.5 * readRequiredDoubleAttributeFromXmlNode("dmGap", matter, "ERROR: No dmGap in Matter tag");
		// внешний радиус оболочки, м:
		double r_cladding = 0.5 * readRequiredDoubleAttributeFromXmlNode("dmCladding", matter, "ERROR: No dmCladding in Matter tag");

		double dz = height * Nz;
		double r_in = 0.;

		// формируем регионы для каждого участка: центральное отверстие, таблетка, газовый зазор, оболочка

		// В Беркуте удалили отверстие из сетки тепловой задачи, оставил закоменченным, вдруг вернут
		//if (!IS_ZERO(r_hole)) {
		//	if (CylindricalCell::is_valid_area(r_in, r_hole, z0, z0 + dz)) {
		//		CylindricalCell cell_hole(r_in, r_hole, z0, z0 + dz);
		//		shared_ptr<StructuralMaterial> MaterialPointer = inputMaterials.find(param.gasName)->second;
		//		DividableCell region_hole(cell_hole, mesh_param.tempInit, 0.0, 0.0, param.gasName, 
		//			MaterialPointer, GasTypeCondition::HOLE, UNDEF_DOUBLE); //TODO: конструктор DividableCell!
		//		region_hole.set_longitudinal_mesh_size(Nz);
		//		region_hole.set_transverse_mesh_size(param.Nr_hole);
		//		heatElement->add_region(region_hole);
		//	}
		//	else {
		//		throw std::invalid_argument("ERROR: invalid mesh for rod's hole: " + heatElement->get_name());
		//	}
		//	
		//}

		r_in = r_hole;

		if (IS_ZERO(r_pellet)) {
			// Если, нет таблетки в Беркуте игнорируется параметры текущего слоя, и особое формирование зазора
            param.Nr_gap = main_param.Nr_pellet + main_param.Nr_gap;
		}
		else {
			if (CylindricalCell::is_valid_area(r_in, r_pellet, z0, z0 + dz)) {
				// таблетка
				CylindricalCell cell_pellet(r_in, r_pellet, z0, z0 + dz);
				shared_ptr<StructuralMaterial> MaterialPointer = inputMaterials.find(mesh_param.fuelMaterial)->second;
				DividableCell region_pellet(cell_pellet, mesh_param.tempInit, 0.0, 1.0, mesh_param.fuelMaterial,
					MaterialPointer, GasTypeCondition::NO_GAS, mesh_param.relDens);
				region_pellet.set_longitudinal_mesh_size(Nz);
				region_pellet.set_transverse_mesh_size(param.Nr_pellet);
				heatElement->add_region(region_pellet);
			} 
			else {
				throw std::invalid_argument("ERROR: invalid mesh for rod's pellet: " + heatElement->get_name());
			}

			r_in = r_pellet;
		}

		if (!IS_ZERO(r_gap)) {
			if (CylindricalCell::is_valid_area(r_in, r_gap, z0, z0 + dz)) {
				// газовый зазор
				CylindricalCell cell_gap(r_in, r_gap, z0, z0 + dz);
				GasTypeCondition gas_type_cond = GasTypeCondition::GAP;
				if (IS_ZERO(r_pellet)) { // отсутствует таблетка, т.е. это газовая  полость - ставим тип как отверстие
					_ASSERT(IS_ZERO(r_hole));
					gas_type_cond = GasTypeCondition::HOLE;
				}
				shared_ptr<StructuralMaterial> MaterialPointer = inputMaterials.find(param.gasName)->second;
				double mat_liq_fraction = 0.0;
				if (LESS_OR_EQ(MaterialPointer->get_MeltTemperature(), mesh_param.tempInit)&&!(MaterialPointer->get_MeltTemperature()==UNDEF_DOUBLE)) {
					mat_liq_fraction = 1.0;
				}
				DividableCell region_gap(cell_gap, mesh_param.tempInit, mat_liq_fraction, 0.0, param.gasName, MaterialPointer,
					gas_type_cond, UNDEF_DOUBLE); //TODO: конструктор DividableCell! Когда будет доработана модель газового зазора - нужно выставить true для GasGap
				region_gap.set_longitudinal_mesh_size(Nz);
				region_gap.set_transverse_mesh_size(param.Nr_gap);
				heatElement->add_region(region_gap);
			}
			else {
				throw std::invalid_argument("ERROR: invalid mesh for rod's gas gap: " + heatElement->get_name());
			}

			r_in = r_gap;
		}
		else {
			// газовый зазор обязателен в Беркуте
			throw std::invalid_argument("ERROR: invalid mesh for rod's gas gap (zero diameter): " + heatElement->get_name());
		}

		if (!IS_ZERO(r_cladding)) {
			if (CylindricalCell::is_valid_area(r_in, r_cladding, z0, z0 + dz)) {
				// оболочка
				CylindricalCell cell_cladding(r_in, r_cladding, z0, z0 + dz);
				shared_ptr<StructuralMaterial> MaterialPointer = inputMaterials.find(mesh_param.claddingMaterial)->second;
				DividableCell region_cladding(cell_cladding, mesh_param.tempInit, 0.0, 0.0, 
                    mesh_param.claddingMaterial, MaterialPointer, GasTypeCondition::NO_GAS, UNDEF_DOUBLE); //TODO: конструктор DividableCell!
				region_cladding.set_longitudinal_mesh_size(Nz);
				region_cladding.set_transverse_mesh_size(param.Nr_cladding);
				heatElement->add_region(region_cladding);
			}
			else {
				throw std::invalid_argument("ERROR: invalid mesh for rod's cladding: " + heatElement->get_name());
			}
		}
		else {
			// оболочка обязательна в Беркуте
			throw std::invalid_argument("ERROR: invalid mesh for rod's cladding (zero diameter): " + heatElement->get_name());
		}

		z0 += dz;

		// Далее расчётную сетку для тепловой структуры сформируем по регионам, в которых уже вручную положили
		// правильное число радиальных и аксиальных ячеек, т.о. аксиальные и радиальные секции не требуются		
	}

	heatElement->add_axial_section(Section(z, z0, cell_number));

	return z0;
}

void InputFileReader::check_materials(const std::vector<std::shared_ptr<MaterialsParam>> &berkut_materials) {
	for (const auto &mat : berkut_materials) {
		auto iMat = inputMaterials.find(mat->mat_name);
		if (iMat == inputMaterials.end()) { // такой материал еще не введён
			shared_ptr<StructuralMaterial> newMaterial;
			SWITCH(mat->mat_name)
			{
				CASE("UO2") :
				{
					newMaterial = make_shared<DataBaseUO2>();
					newMaterial->setRelDens(mat->rel_dens);
					newMaterial->setBurnUp(mat->burn_up);
					break;
				}

				CASE("UPN") :
				{
					newMaterial = make_shared<DataBaseUPuN>();
					newMaterial->setRelDens(mat->rel_dens);
					newMaterial->setBurnUp(mat->burn_up);
					newMaterial->setPu(mat->pu_part);
					break;
				}

				CASE("MOX") :
				{
					newMaterial = make_shared<DataBaseMOX>();
					newMaterial->setRelDens(mat->rel_dens);
					newMaterial->setBurnUp(mat->burn_up);
					newMaterial->setPu(mat->pu_part);
					break;
				}

				CASE("B4C") :
				{
					newMaterial = make_shared<DataBaseB4C>();
					break;
				}

				CASE("EK164") :
				{
					newMaterial = make_shared<DataBaseEK164>();
					break;
				}

                CASE("EP450") :
                {
                    newMaterial = make_shared<DataBaseEP450>();
                    break;
                }

                CASE("EP450DUO") :
                {
                    newMaterial = make_shared<DataBaseEP450DUO>();
                    break;
                }

				CASE("Ar") :
				{
					newMaterial = make_shared<DataBaseGas>("Ar");
					newMaterial->setGasPressure(mat->gas_pressure);
					break;
				}

				CASE("He") :
				{
					newMaterial = make_shared<DataBaseGas>("He");
					newMaterial->setGasPressure(mat->gas_pressure);
					break;
				}

				CASE("N2") :
				{
					newMaterial = make_shared<DataBaseGas>("N2");
					newMaterial->setGasPressure(mat->gas_pressure);
					break;
				}

				CASE("Na") : // использует только пар натрия
				{
					newMaterial = make_shared<DataBaseCoolantVapor>("Na");
					newMaterial->setGasPressure(mat->gas_pressure);
					break;
				}

				DEFAULT :
				{ // здесь не проводится проверка, что материал существует в базе данных, это будет произведено в конструкторе DataBaseTempDependent
					newMaterial = make_shared<DataBaseTempDependent>(mat->mat_name);
				}
			}

			newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
			inputMaterials.insert(pair<string, shared_ptr<StructuralMaterial>>(mat->mat_name, newMaterial));
		}
		else {
			if (mat->mat_name == "UO2" || mat->mat_name == "UPN" || mat->mat_name == "MOX") {
				iMat->second->setRelDens(mat->rel_dens);
				if (iMat->second->getBurnUp() == UNDEF_DOUBLE) { // не было вввода выгорания в разделе SAFR
					iMat->second->setBurnUp(0.0);
				}
			}

			if (mat->mat_name == "UPN" || mat->mat_name == "MOX") {
				iMat->second->setPu(mat->pu_part);
			}
			

			if (mat->mat_name == "Ar" || mat->mat_name == "He") {
				iMat->second->setGasPressure(mat->gas_pressure);
			}
		}
	}
}