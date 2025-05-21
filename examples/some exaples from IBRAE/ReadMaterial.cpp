//materials
#include "InputFileReader.h"
#include "../Materials/DataBaseMaterial.h"
#include "../Materials/PredatorMaterial.h"
#include "../Materials/str_switch.h"

#include "../Materials/UraniumSteelEutectic.h"

extern string DirService;

bool InputFileReader::readMaterials(const string& module_name) {
	auto module_name_size = module_name.size() + 1;
    string DatabaseFile = DirService.substr(0, DirService.size() - module_name_size) + material_data_base_file_name;
    if (!mpInitDatabase(DatabaseFile.c_str())) {
        throw std::invalid_argument("DataBaseMaterial dir error");
    }

	std::vector<std::string> eutectic_names;
	for(auto& current_eut: eutectic_vector) {
	    eutectic_names.push_back(current_eut->name_material_eutectic());
	}

	for (pugi::xml_node material = root.child("MatProp"); material; material = material.next_sibling("MatProp")) {
		if (!isOptionalFlagSwitchedOnInXmlNode("Default", material)) {
		    std::string name = readRequiredStringAttributeFromXmlNode("Name", material, "ERROR: No name in MatProp");
			bool is_eutectic = false;
            for (auto& eut_name : eutectic_names) {
                if (name == eut_name) {
                    shared_ptr<UraniumSteelEutectic> inputMaterial = make_shared<UraniumSteelEutectic>();

                    if (!readMaterial(material, inputMaterial)) {
                        return false;
                    }

                    inputMaterials.insert(pair <string, shared_ptr<UraniumSteelEutectic>>(inputMaterial->name, inputMaterial));
                    is_eutectic = true;
                }
            }

            if (!is_eutectic) {
                shared_ptr<InputMaterial> inputMaterial = make_shared<InputMaterial>();

                if (!readMaterial(material, inputMaterial)) {
                    return false;
                }

                inputMaterials.insert(pair <string, shared_ptr<InputMaterial>>(inputMaterial->name, inputMaterial));
            }
		}
		else { //материал задан в базе данных
			string name = readRequiredStringAttributeFromXmlNode("Name", material, "ERROR: No name in MatProp");

			shared_ptr<DataBaseMaterial> newMaterial;

			if (!isOptionalFlagSwitchedOnInXmlNode("PreCalc", material, true)) {
				isSmartDB_used = true; //выставляем флаг, что база данных используется. Необходимо для параллельной версии

				SWITCH(name)
				{
					CASE("UO2") :
					{
						newMaterial = make_shared<DataBaseUO2>();
						// RelDens и BurnUp пока необязательные, проверка проводиться позже:
						newMaterial->setRelDens(readOptionalDoubleAttributeFromXmlNode("RelDens", material, UNDEF_DOUBLE));
						newMaterial->setBurnUp(readOptionalDoubleAttributeFromXmlNode("BurnUp", material, UNDEF_DOUBLE));
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("EK164") :
					{
						newMaterial = make_shared<DataBaseEK164>();
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("EP450") :
					{
						newMaterial = make_shared<DataBaseEP450>();
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}
				    
				    CASE("EP450DUO") :
					{
						newMaterial = make_shared<DataBaseEP450DUO>();
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("UPN") :
					{
						newMaterial = make_shared<DataBaseUPuN>();
						// RelDens, Pu и BurnUp пока необязательные, проверка проводиться позже:
						newMaterial->setRelDens(readOptionalDoubleAttributeFromXmlNode("RelDens", material, UNDEF_DOUBLE));
						newMaterial->setBurnUp(readOptionalDoubleAttributeFromXmlNode("BurnUp", material, UNDEF_DOUBLE));
						newMaterial->setPu(readOptionalDoubleAttributeFromXmlNode("Pu", material, UNDEF_DOUBLE));
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("UN") :
					{
						newMaterial = make_shared<DataBaseUN>();
						// RelDens, Pu и BurnUp пока необязательные, проверка проводиться позже:
						newMaterial->setRelDens(readOptionalDoubleAttributeFromXmlNode("RelDens", material, UNDEF_DOUBLE));
						newMaterial->setBurnUp(readOptionalDoubleAttributeFromXmlNode("BurnUp", material, UNDEF_DOUBLE));
						newMaterial->setPu(0.0);
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("MOX") :
					{
						newMaterial = make_shared<DataBaseMOX>();
						// RelDens, Pu и BurnUp пока необязательные, проверка проводиться позже:
						newMaterial->setRelDens(readOptionalDoubleAttributeFromXmlNode("RelDens", material, UNDEF_DOUBLE));
						newMaterial->setBurnUp(readOptionalDoubleAttributeFromXmlNode("BurnUp", material, UNDEF_DOUBLE));
						newMaterial->setPu(readOptionalDoubleAttributeFromXmlNode("Pu", material, UNDEF_DOUBLE));
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("B4C") :
					{
						newMaterial = make_shared<DataBaseB4C>();
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("Ar") :
					{
						newMaterial = make_shared<DataBaseGas>("Ar");
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("He") :
					{
						newMaterial = make_shared<DataBaseGas>("He");
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("N2") :
					{
						newMaterial = make_shared<DataBaseGas>("N2");
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("Na") : // используем пары натрия
					{
						newMaterial = make_shared<DataBaseCoolantVapor>("Na");
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("Pb") : // используем пары свинца
					{
						newMaterial = make_shared<DataBaseCoolantVapor>("Pb");
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

				    DEFAULT:
					{ // здесь не проводится проверка, что материал существует в базе данных, это будет произведено в конструкторе DataBaseTempDependent
						newMaterial = make_shared<DataBaseTempDependent>(name);
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
					}
				}
			} else {
				double temp_step = readOptionalDoubleAttributeFromXmlNode("TempStep", material, PreCalcDataBaseMaterialUtils::default_temp_step);

				SWITCH(name)
				{
					CASE("UO2") :
					{
						newMaterial = make_shared<PreCalcDataBaseUO2>(temp_step);
						// RelDens и BurnUp пока необязательные, проверка проводиться позже:
						newMaterial->setRelDens(readOptionalDoubleAttributeFromXmlNode("RelDens", material, UNDEF_DOUBLE));
						newMaterial->setBurnUp(readOptionalDoubleAttributeFromXmlNode("BurnUp", material, UNDEF_DOUBLE));
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("EK164") :
					{
						newMaterial = make_shared<PreCalcDataBaseEK164>(temp_step);
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("EP450") :
					{
						newMaterial = make_shared<PreCalcDataBaseEP450>(temp_step);
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("EP450DUO") :
					{
						newMaterial = make_shared<PreCalcDataBaseEP450DUO>(temp_step);
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("UPN") :
					{
						newMaterial = make_shared<PreCalcDataBaseUPuN>(temp_step);
						// RelDens, Pu и BurnUp пока необязательные, проверка проводиться позже:
						newMaterial->setRelDens(readOptionalDoubleAttributeFromXmlNode("RelDens", material, UNDEF_DOUBLE));
						newMaterial->setBurnUp(readOptionalDoubleAttributeFromXmlNode("BurnUp", material, UNDEF_DOUBLE));
						newMaterial->setPu(readOptionalDoubleAttributeFromXmlNode("Pu", material, UNDEF_DOUBLE));
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("UN") :
					{
						newMaterial = make_shared<PreCalcDataBaseUN>(temp_step);
						// RelDens, Pu и BurnUp пока необязательные, проверка проводиться позже:
						newMaterial->setRelDens(readOptionalDoubleAttributeFromXmlNode("RelDens", material, UNDEF_DOUBLE));
						newMaterial->setBurnUp(readOptionalDoubleAttributeFromXmlNode("BurnUp", material, UNDEF_DOUBLE));
						newMaterial->setPu(0.0);
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("MOX") :
					{
						newMaterial = make_shared<PreCalcDataBaseMOX>(temp_step);
						// RelDens, Pu и BurnUp пока необязательные, проверка проводиться позже:
						newMaterial->setRelDens(readOptionalDoubleAttributeFromXmlNode("RelDens", material, UNDEF_DOUBLE));
						newMaterial->setBurnUp(readOptionalDoubleAttributeFromXmlNode("BurnUp", material, UNDEF_DOUBLE));
						newMaterial->setPu(readOptionalDoubleAttributeFromXmlNode("Pu", material, UNDEF_DOUBLE));
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("B4C") :
					{
						newMaterial = make_shared<PreCalcDataBaseB4C>(temp_step);
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("Ar") :
					{
						double press_step = readOptionalDoubleAttributeFromXmlNode("PressStep", material, PreCalcDataBaseMaterialUtils::default_press_step);
						double enth_step = readOptionalDoubleAttributeFromXmlNode("EnthStep", material, PreCalcDataBaseMaterialUtils::default_enth_step);
						newMaterial = make_shared<PreCalcDataBaseGas>("Ar", temp_step, enth_step, press_step);
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("He") :
					{
						double press_step = readOptionalDoubleAttributeFromXmlNode("PressStep", material, PreCalcDataBaseMaterialUtils::default_press_step);
						double enth_step = readOptionalDoubleAttributeFromXmlNode("EnthStep", material, PreCalcDataBaseMaterialUtils::default_enth_step);
						newMaterial = make_shared<PreCalcDataBaseGas>("He", temp_step, enth_step, press_step);
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("N2") :
					{
						double press_step = readOptionalDoubleAttributeFromXmlNode("PressStep", material, PreCalcDataBaseMaterialUtils::default_press_step);
						double enth_step = readOptionalDoubleAttributeFromXmlNode("EnthStep", material, PreCalcDataBaseMaterialUtils::default_enth_step);
						newMaterial = make_shared<PreCalcDataBaseGas>("N2", temp_step, enth_step, press_step);
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("Na") : // используем пары натрия
					{
						double press_step = readOptionalDoubleAttributeFromXmlNode("PressStep", material, PreCalcDataBaseMaterialUtils::default_press_step);
						newMaterial = make_shared<PreCalcDataBaseCoolantVapor>("Na", temp_step, press_step);
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

					CASE("Pb") : // используем пары свинца
					{
						double press_step = readOptionalDoubleAttributeFromXmlNode("PressStep", material, PreCalcDataBaseMaterialUtils::default_press_step);
						newMaterial = make_shared<PreCalcDataBaseCoolantVapor>("Pb", temp_step, press_step);
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
						break;
					}

				    DEFAULT:
					{ // здесь не проводится проверка, что материал существует в базе данных, это будет произведено в конструкторе DataBaseTempDependent
						newMaterial = make_shared<PreCalcDataBaseTempDependent>(name, temp_step);
						newMaterial->setMeltFlowability(MeltFlowability::DISABLE);
						inputMaterials.insert(pair <string, shared_ptr<DataBaseMaterial>>(name, newMaterial));
					}
				}
			}

			bool is_melt_temperature_in_input_file = checkOptionalAttributeName("MeltTemp", material);
			if (is_melt_temperature_in_input_file) {
				// несмотря на то, что материал из базы данных, температура плавления задается во входном файле
				string error_message = "Fault of attribute MeltTemp";
				double melt_temperature = readRequiredDoubleAttributeFromXmlNode("MeltTemp", material, error_message);

				newMaterial->set_melt_temperature(melt_temperature);
			}
		}
	}

	// фиктивный материал для исчезнувших материальных слоев
	auto newMaterial = make_shared<PredatorMaterial>();
	inputMaterials.insert(pair <string, shared_ptr<PredatorMaterial>>("PredatorMaterial", newMaterial));

	// проверка, что не больше одной внешней и не больше одной внутренней возможности (плёнки) стекания материала
	size_t external_count = 0;
	size_t internal_count = 0;
	for (const auto &mat : inputMaterials){
		switch (mat.second->getMeltFlowability()) {
			case MeltFlowability::EXTERNAL :
				external_count++;
				break;
			case MeltFlowability::INTERNAL :
				internal_count++;
				break;
			default:
				break;
		}
	}

	/*
	if (external_count > 1) {
		throw std::invalid_argument("ERROR: More than one film at the external boundary: pay attention to MeltFlow argument");
	}

	if (internal_count > 1) {
		throw std::invalid_argument("ERROR: More than one film at the internal boundary: pay attention to MeltFlow argument");
	}
	*/

	return true;
}

MeltFlowability InputFileReader::readMeltFlowability(pugi::xml_node material) {
    string melt_flow;
    MeltFlowability melt_flow_type = MeltFlowability::DISABLE; // значние по умолчанию
    if (!material.attribute("MeltFlow").empty()) {
        melt_flow = material.attribute("MeltFlow").as_string();
        if (melt_flow == "Off") {
            melt_flow_type = MeltFlowability::DISABLE;
        }
        else if (melt_flow == "Internal") {
            melt_flow_type = MeltFlowability::INTERNAL;
        }
        else if (melt_flow == "External") {
            melt_flow_type = MeltFlowability::EXTERNAL;
        }
        else {
            throw std::invalid_argument("ERROR: Invalid argument for MeltFlow: " + melt_flow);
        }
    }

    return melt_flow_type;
}

bool InputFileReader::readMaterial(pugi::xml_node material, shared_ptr<InputMaterial> inputMaterial) {
	inputMaterial->name = readRequiredStringAttributeFromXmlNode("Name", material, "ERROR: No name in MatProp");
	inputMaterial->setMeltFlowability(MeltFlowability::DISABLE);

	double melt_temp = readRequiredDoubleAttributeFromXmlNode("MeltTemp", material, "There is no melt temperature for material " + inputMaterial->name);
	inputMaterial->setMeltTemperature(melt_temp);

    double boil_temp = readOptionalDoubleAttributeFromXmlNode("BoilTemp", material, 10000);
    inputMaterial->setBoilTemperature(boil_temp);

	double vaporization_enthalpy = readOptionalDoubleAttributeFromXmlNode("VaporizationEnthalpy", material, 0.);
	inputMaterial->setVaporizationEnthalpy(vaporization_enthalpy);

	double molar_mass = readOptionalDoubleAttributeFromXmlNode("MolarMass", material, UNDEF_DOUBLE);
	inputMaterial->setMolarMass(molar_mass);

	double melt_enth;
	bool flag_melt_enth = false;
	if (!material.attribute("MeltEnth").empty()) {
		melt_enth = material.attribute("MeltEnth").as_double();
		flag_melt_enth = true;
	}
	if (flag_melt_enth) {
		inputMaterial->setMeltEnthalpy(melt_enth);
	}
	
	//--------------------------Main--------------------------------//
	inputMaterial->enthalpyTable = readOptionalMaterialProperty(material, "Enthalpy", melt_temp, inputMaterial->name);
	if (!inputMaterial->enthalpyTable.empty()) {
		if (!inputMaterial->check_table(inputMaterial->enthalpyTable)) {
			throw std::invalid_argument("Material " + inputMaterial->name + ": Not enough data for enthalpy");
		}
	}

	inputMaterial->heatCapacityTable = readOptionalMaterialProperty(material, "HeatCapacity", melt_temp, inputMaterial->name);
	if (!inputMaterial->heatCapacityTable.empty()) {
		if (!inputMaterial->check_table(inputMaterial->heatCapacityTable)) {
			throw std::invalid_argument("Material " + inputMaterial->name + ": Not enough data for heat capacity");
		}
	}

	if (inputMaterial->enthalpyTable.empty() && inputMaterial->heatCapacityTable.empty()) {
		throw std::invalid_argument("There is no enthalpy or heat capacity for material " + inputMaterial->name);
	}

	if (!inputMaterial->enthalpyTable.empty() && !inputMaterial->heatCapacityTable.empty()) {
		throw std::invalid_argument("Material " + inputMaterial->name + ": Inputting both, enthalpy and heat capacity is not allowed");
	}

	if (inputMaterial->enthalpyTable.empty() && !inputMaterial->heatCapacityTable.empty()) {
		//расчёт энтальпии
		if (flag_melt_enth) {
			inputMaterial->build_enthalpy_by_heat_capacity();
		}
		else {
			throw std::invalid_argument("Material " + inputMaterial->name + ": No melt enthalpy");
		}
	}

	if (!inputMaterial->enthalpyTable.empty() && inputMaterial->heatCapacityTable.empty()) {
		//расчёт теплоёмкости
		inputMaterial->build_heat_capacity_by_enthalpy();
	}

	//-------------------Required parameters-------------------------//
	inputMaterial->heatCondTable = readRequiredMaterialProperty(material, "HeatCond", melt_temp, inputMaterial->name);
	inputMaterial->densityTable = readRequiredMaterialProperty(material, "Dens", melt_temp, inputMaterial->name);
	inputMaterial->viscosityTable = readRequiredMaterialProperty(material, "Viscosity", melt_temp, inputMaterial->name);

	//-------------------Optional parameters-------------------------//
	inputMaterial->emissivityTable = readOptionalMaterialProperty(material, "Emissivity", melt_temp, inputMaterial->name);
	inputMaterial->surfaceTensionTable = readOptionalMaterialProperty(material, "SurfaceTension", melt_temp, inputMaterial->name);
	inputMaterial->pressureSatTable = readOptionalMaterialProperty(material, "PressureSat", melt_temp, inputMaterial->name);
	// считываем контактный угол в градусах
	inputMaterial->contactAngleTable = readOptionalMaterialProperty(material, "ContactAngle", melt_temp, inputMaterial->name);
	// и переводим в косинус
	if(!inputMaterial->contactAngleTable.empty()) {
		size_t size = inputMaterial->contactAngleTable.size();
		for (size_t i = 0; i < size; ++i) {
			if (LESS(inputMaterial->contactAngleTable[i].second, 0) && LESS(180, inputMaterial->contactAngleTable[i].second)) {
				throw std::invalid_argument("Wrong input data for ContactAngle for material " + inputMaterial->name);

			}
			inputMaterial->contactAngleTable[i].second = cos(inputMaterial->contactAngleTable[i].second / 180 * M_PI);
		}
	}

	inputMaterial->thermalExpansionTable = readOptionalMaterialProperty(material, "ThermalExpansion", melt_temp, inputMaterial->name);
	
	if(!errorMessage.empty()) {
		throw std::invalid_argument(errorMessage + "Material name is " + inputMaterial->name);
	}

	return true;
}

vector <pair<double, double>> InputFileReader::readOptionalMaterialProperty(pugi::xml_node material, const pugi::char_t *prop, const double &temp_melt, const string &material_name) {
	vector <std::pair<double, double>> table;
	size_t melt_point = 0;
	double prev_temp = 0.0;
	for (pugi::xml_node property = material.child(prop); property; property = property.next_sibling(prop)) {
		table.push_back(readMaterialPropertyLine(property));
		if (IS_ZERO(table.back().first - temp_melt)) {
			melt_point++;
			if (melt_point > 2 || !LESS_OR_EQ(prev_temp, table.back().first)) {
				throw std::invalid_argument("Wrong input data for " + string(prop) + " for material " + material_name);
			}
			prev_temp = table.back().first;
		}
		else {
			if (!LESS(prev_temp, table.back().first)) {
				throw std::invalid_argument("Wrong input data for " + string(prop) + " for material " + material_name);
			}
			prev_temp = table.back().first;
		}
	}

	return table;
}

vector <pair<double, double>> InputFileReader::readRequiredMaterialProperty(pugi::xml_node material, const pugi::char_t *prop, const double &temp_melt, const string &material_name) {
	auto table = readOptionalMaterialProperty(material, prop, temp_melt, material_name);
	if (table.empty()) {
		throw std::invalid_argument("There is no " + string(prop) + " table for material " + material_name);
	}

	return table;
}

std::pair<double, double> InputFileReader::readMaterialPropertyLine(pugi::xml_node matProp) {
	return make_pair<double, double>( readRequiredDoubleAttributeFromXmlNode("Temp", matProp, "No Temp in MatProp")
									, readRequiredDoubleAttributeFromXmlNode("Coef", matProp, "No Coef in MatProp")
								    );
}
