#include "InputFileReader.h"
#include "../HeatBound/HeatBoundCell.h"
#include "../RadiationExchange/RadiationExchangeViewFactor.h"
#include "../RadiationExchange/RadiationExchange.h"
#include "../MeltPool/MeltPool.h"
#include "../Dissociation/FreeConvection.h"
#include "../Dissociation/ForcedConvection.h"
#include "../KERNEL/HeatModule/HeatModule.h"
#include "../Utils/utils.h"
#include "../SAFR_DLL/headers/SAFR_Hydra.h"

#include <omp.h>

// для определения имени тега с данными твэльного модуля
extern std::string ModuleName;

const string InputFileReader::flag_ON = "ON";
const string InputFileReader::flag_OFF = "OFF";
const std::string InputFileReader::material_data_base_file_name = "SmartDB.xml";
const std::string InputFileReader::fission_product_data_base_file_name = "Aerosols.xml";

std::string InputFileReader::getErrorMessage()const {
	return errorMessage;
}

std::vector <shared_ptr<HeatElement>> InputFileReader::getHeatElements() {
	return inputHeatElements;
}

std::vector <std::shared_ptr<MeltPoolLayer>> InputFileReader::getMeltPoolLayers() {
	return meltPoolLayers;
}


bool InputFileReader::checkMeltPoolLayerByName(string& mpl_name) {
	for (size_t i = 0; i < meltPoolLayers.size(); ++i) {
		if (meltPoolLayers[i]->get_name() == mpl_name) {
			return true;
		}
	}
	return false;
}

std::vector <std::shared_ptr<MeltPool>> InputFileReader::getMeltPools() {
	return meltPools;
}

shared_ptr<Atmosphere_state> InputFileReader::getAtmosphereState() {
	return atmosphere_state; 
}

shared_ptr<Atmosphere_state> InputFileReader::getAtmosphereStateByName(string &atm_name) {
	shared_ptr<Atmosphere_state> needed_atmosphere_state = nullptr;
	for (size_t i = 0; i < atmosphere_states.size(); ++i) {
		if (atmosphere_states[i]->get_name() == atm_name) {
			needed_atmosphere_state = atmosphere_states[i];
		}
	}
	return needed_atmosphere_state;
}

bool InputFileReader::checkAtmosphereStateByName(string& atm_name) {
	for (size_t i = 0; i < atmosphere_states.size(); ++i) {
		if (atmosphere_states[i]->get_name() == atm_name) {
			return true;
		}
	}
	return false;
}

std::vector <std::shared_ptr<Atmosphere_state>> InputFileReader::getAtmosphereStates() {
	return atmosphere_states;
}

InputFileReader::~InputFileReader() {}

std::map <std::string, shared_ptr<StructuralMaterial>> InputFileReader::getStructMaterials() {
	return inputMaterials;
}

std::map < std::string, std::shared_ptr<FissionProduct>> InputFileReader::getFissionProducts() {
	return fissionProducts;
}

std::vector <shared_ptr<timeControlRow>> InputFileReader::getTimeControlTable() {
	return timeControlTable;
}

std::shared_ptr<Core> InputFileReader::getCore() {
	return inputCore;
}

std::vector<std::vector<std::shared_ptr<RadiationExchangeViewFactor>>> InputFileReader::getRadiationExchangeViewFactorsGroup() {
	return radiationExchangeViewFactorsGroup;
}

shared_ptr<DissociationModel> InputFileReader::getDissociation() {
	return inputDiss;
}

shared_ptr<HeatModuleExtraOptions> InputFileReader::getHeatModuleExtraOptions() {
    return extra_options;
}

shared_ptr<SecondaryCriticality> InputFileReader::getSecondaryCriticality() {
	return secondary_criticality;
}

shared_ptr<UN_O_Interaction::UN_O_InteractionModel> InputFileReader::getUN_O_InteractionModel() {
	return un_o_model;
}

vector<shared_ptr<UN_O_Interaction::UN_O_InteractionModel_HeatStruct_Link>> InputFileReader::getUN_O_InteractionModel_HeatStruct_Links() {
	return un_o_h_struct_link;
}

#ifdef _WIN32
std::shared_ptr<Dissolution> InputFileReader::getUO2_DissolutionModel() {
    return uo2_dissolution;
}
#endif

std::vector<std::shared_ptr<EutecticBase>> InputFileReader::getInputEutectics() {
    return eutectic_vector;
}

bool InputFileReader::readData(const pugi::xml_node &task, const string& module_name) {
	auto main = getRequiredChildInXmlNode("Main", task);
    if (!readCalculation(main)) {
        throw std::invalid_argument("ERROR: Not Read TimeControl");
    }

	isActiveHydraulics = SAFR_Hydra_Active;
	isActiveNeutronics = SAFR_DN3D_Active;

	bool readOK = true;

	if (!task.child("Core").empty()) {
		root = task.child("Core");
		bool readCoreOK = readCore();
		readOK = readOK && readCoreOK;
	}
    else if (!task.child("LinkChannelHS").empty()) {
        pugi::xml_node LinkHS = task.child("LinkChannelHS");
        bool readLinksOK = readLinkChannelHS(LinkHS);
        readOK = readOK && readLinksOK;
    }

	root = getRequiredChildInXmlNode(module_name, task);
	readOK = readRoot(module_name);

    std::string BerkutTag;
	if (ModuleName.find("SAFR") != std::string::npos) {
		BerkutTag = "BERKUT";   // для ЕВКЛИДа
	}

    if (	ModuleName.find("SACURA")      != std::string::npos
        ||  ModuleName.find("HEFEST_CORE") != std::string::npos) {
        BerkutTag = "FuelRods"; // для ТИТАНа
    }

	root = task.child(BerkutTag.c_str());
	if (!root.empty()) {
        bool readBERKUT = readRootInBERKUTFormat(fuelRodList);
		readOK = readOK && readBERKUT;
	}

	return readOK;
}

bool InputFileReader::readRoot(const string& module_name) {

	readParallel();

	readNumericalParameters();

	readTables();

	readEutectics();

	readMaterials(module_name);

	readFissionProducts(module_name);

	readFuelRodList();

	readCanisterList();

	readAbsorbingRodList();

	readNuclides();

	readHeatStructs();

	readHeatBounds();

	readFilmBounds();

	readGasFilmBound();

	readMeltPools();

	readAtmosphereStates(root);

	readMeltPoolLayers(root);

	readConnections();

	readDissociation();

	readHeatModuleExtraOptions();

	readHeatStructInterTransfer();

	HeatStruct_ChangeFilmFlowability();

	readAxialTransfer();

	readCoreCatcherStratification();

	read_UN_Pb_Steel_Ar_O2_interaction();

	createConnections();

	clearingCore();

	readSecondaryCriticality();
#ifdef _WIN32
	read_UO2_Dissolution();
#endif

	return true;
}

bool InputFileReader::createConnections() {
	bool is_connected_to_HYDRA_internal = false;
	bool is_connected_to_HYDRA_external = false;
	bool is_connected_to_GasBound_internal = false;
	bool is_connected_to_GasBound_external = false;
	bool is_connected_up = false;
	bool is_connected_down = false;

	for (const auto &connection : connections) {
		if (connection->type == ConnectionType::DEFAULT) {
			if (connection->TypeFrom == "FuelRod" || connection->TypeFrom == "AbsorbingRod")
				continue;

			if (connection->TypeFrom != "HeatStruct") {
				throw std::invalid_argument("ERROR: Invalid connection in TypeFrom " + '"' + connection->TypeFrom + '"' + " is not allowed");
			}

			unsigned int bound_type = UNDEF_SIZE;
			if (connection->Boundary == "Upper") {
				bound_type = HeatElement::UPPER;
			}
			else if (connection->Boundary == "Down") {
				bound_type = HeatElement::DOWN;
			}
			else if (connection->Boundary == "Internal") {
				bound_type = HeatElement::INTERNAL;
			}
			else if (connection->Boundary == "External") {
				bound_type = HeatElement::EXTERNAL;
			}
			else if (connection->Boundary == "BottomHeatBound") {
				bound_type = HeatElement::BOTTOM_HEAT_BOUND;
			}
			else if (connection->Boundary == "UpperHeatBound") {
				bound_type = HeatElement::UPPER_HEAT_BOUND;
			}
			else {
				throw std::invalid_argument("ERROR: InputFileReader::createConnections: Invalid boundary type: " + connection->Boundary);
			}

			auto h_element = find_object_by_name_or_fail<HeatElement>(connection->NameFrom, inputHeatElements);
			if (connection->TypeTo == "MeltPool") {
				auto pool = find_object_by_name_or_fail<MeltPool>(connection->NameTo, meltPools);

				auto film_bound = make_shared<FilmBound>("FilmBoundFree_" + pool->get_name(), BoundaryType::FREE);
				if (bound_type == HeatElement::UPPER) {
					pool->add_heat_struct_down(h_element);
				}
				else if (bound_type == HeatElement::DOWN) {
					pool->add_pool_data(h_element);
					//pool->add_heat_struct_up(h_element);
				}
				else {
					throw std::invalid_argument("ERROR: InputFileReader::createConnections: Invalid boundary type: " + connection->Boundary);
				}

				h_element->setBoundary(film_bound, bound_type);
			}
			else if (connection->TypeTo == "FilmBound") {
				auto film_bound = find_object_by_name_or_fail<FilmBound>(connection->NameTo, filmBounds);
				h_element->setBoundary(film_bound, bound_type);
			}
			else if (connection->TypeTo == "GasBound") {
				if (is_connected_to_HYDRA_external && (bound_type == HeatElement::EXTERNAL)) {
					throw std::invalid_argument("ERROR: Invalid connection: Incorrect boundary condition for external boundary of HeatStruct: " + h_element->get_name() + "\n");
				}

				if (is_connected_to_HYDRA_internal && (bound_type == HeatElement::INTERNAL)) {
					throw std::invalid_argument("ERROR: Invalid connection: Incorrect boundary condition for internal boundary of HeatStruct: " + h_element->get_name() + "\n");
				}

				auto gas_bound = find_object_by_name_or_fail<GasBound>(connection->NameTo, gasFilmBounds);
				h_element->setBoundary(gas_bound, bound_type);

				is_connected_to_GasBound_internal = (bound_type == HeatElement::INTERNAL);
				is_connected_to_GasBound_external = (bound_type == HeatElement::EXTERNAL);
			}
			else { // канал и тепловое граничное условие
				shared_ptr<HeatBound> heat_bound;
				if (connection->TypeTo == "HeatBound") {
					auto heat_bound_in_base = find_object_by_name_or_fail<HeatBound>(connection->NameTo, heatBoundsInInputFile);

					// создаём новое гран. условие для избежания влияния изменения текущего гран условия на другие теплов. структуры
					heat_bound = make_shared<HeatBound>(heat_bound_in_base->get_name(), heat_bound_in_base->get_type());
					// копируем параметры гран. условия из списка в новое гран условие для текущей тепл. структуры
					*heat_bound = *heat_bound_in_base;

					is_connected_up = (bound_type == HeatElement::UPPER_HEAT_BOUND);
					is_connected_down = (bound_type == HeatElement::BOTTOM_HEAT_BOUND);
				}
				else if (connection->TypeTo == "Channel") {
					if (is_connected_to_GasBound_internal && (bound_type == HeatElement::INTERNAL)) {
						throw std::invalid_argument("ERROR: Invalid connection: Incorrect boundary condition for internal boundary of HeatStruct: " + h_element->get_name() + "\n");
					}

					if (is_connected_to_GasBound_external && (bound_type == HeatElement::EXTERNAL)) {
						throw std::invalid_argument("ERROR: Invalid connection: Incorrect boundary condition for external boundary of HeatStruct: " + h_element->get_name() + "\n");
					}

					string bound_name = connection->Boundary + "_Channel_" + connection->NameTo;

					auto gb = make_shared<GasBound>(bound_name, GasBoundaryType::HYDRA_CELL);
					h_element->setBoundary(gb, bound_type);

					is_connected_to_HYDRA_internal = (bound_type == HeatElement::INTERNAL);
					is_connected_to_HYDRA_external = (bound_type == HeatElement::EXTERNAL);

					size_t cell_number = h_element->calc_axial_number_by_section(); // количество ячеек вдоль теплового элемента
					heat_bound = make_shared<HeatBoundCell>(bound_name, HeatBound::TYPE_T_ENV_EX, cell_number);
					heat_bound->set_connection_type(HeatBound::TYPE_CHANNEL);
				}
				else if (connection->TypeTo == "Region3D") {
					if (is_connected_to_GasBound_internal && (bound_type == HeatElement::INTERNAL)) {
						throw std::invalid_argument("ERROR: Invalid connection: Incorrect boundary condition for internal boundary of HeatStruct: " + h_element->get_name() + "\n");
					}

					if (is_connected_to_GasBound_external && (bound_type == HeatElement::EXTERNAL)) {
						throw std::invalid_argument("ERROR: Invalid connection: Incorrect boundary condition for external boundary of HeatStruct: " + h_element->get_name() + "\n");
					}

					string bound_name = connection->Boundary + "_Region3D_" + connection->NameTo;

					auto gb = make_shared<GasBound>(bound_name, GasBoundaryType::HYDRA_CELL);
					h_element->setBoundary(gb, bound_type);

					is_connected_to_HYDRA_internal = (bound_type == HeatElement::INTERNAL);
					is_connected_to_HYDRA_external = (bound_type == HeatElement::EXTERNAL);

					size_t cell_number = h_element->calc_axial_number_by_section(); // количество ячеек вдоль теплового элемента
					heat_bound = make_shared<HeatBoundCell>(bound_name, HeatBound::TYPE_T_ENV_EX, cell_number);
					heat_bound->set_connection_type(HeatBound::TYPE_3D_CHANNEL);
				}
				else if (connection->TypeTo == "ChannelHeatSource") { // граничное условие через ChannelHeatSource
					if (is_connected_down && (bound_type == HeatElement::BOTTOM_HEAT_BOUND)) {
						throw std::invalid_argument("ERROR: Invalid connection: Incorrect boundary condition for down boundary of HeatStruct: " + h_element->get_name() + "\n");
					}

					if (is_connected_up && (bound_type == HeatElement::UPPER_HEAT_BOUND)) {
						throw std::invalid_argument("ERROR: Invalid connection: Incorrect boundary condition for up boundary of HeatStruct: " + h_element->get_name() + "\n");
					}
					string bound_name = connection->Boundary + "_ChannelHeatSource_" + connection->NameTo;
					heat_bound = make_shared<HeatBound>(bound_name, HeatBound::TYPE_T_ENV_EX_HEAT_SOURCE);

					is_connected_up = (bound_type == HeatElement::UPPER_HEAT_BOUND);
					is_connected_down = (bound_type == HeatElement::BOTTOM_HEAT_BOUND);
				}
				else {
					throw std::invalid_argument("ERROR: Invalid connection type " + connection->TypeTo + " for connection to HeatStruct " + h_element->get_name());
				}

				if (!h_element->isHeatBoundEmpty(bound_type)) {
					h_element->tryToCombineBoundaries(heat_bound, bound_type);
				}
				else {
					h_element->setBoundary(heat_bound, bound_type);
				}
			}
		}
		else if (connection->type == ConnectionType::UN_O_CONNECTION) {
			auto un_connection = std::dynamic_pointer_cast<UN_O_Connection>(connection);
			if (un_o_model->name != un_connection->UN_O_InteractionModel) {
				throw std::invalid_argument("ERROR: Invalid connection: UN_O_InteractionModel with name: " + un_connection->UN_O_InteractionModel + " not found for connection to HeatStruct " + un_connection->UN_O_InteractionModel);
			}

			auto h_element = find_object_by_name_or_fail<HeatElement>(un_connection->NameHeatStruct, inputHeatElements);

			auto link = make_shared<UN_O_Interaction::UN_O_InteractionModel_HeatStruct_Link>(un_connection->UN_O_InteractionModel, un_connection->NameHeatStruct);
			un_o_h_struct_link.push_back(link);
		}
	}

	return true;
}

void InputFileReader::readNumericalParameters() {
	if (root.child("NumericalParameters").empty()) {
		return;
	}

	pugi::xml_node num_param = root.child("NumericalParameters");

	if (!num_param.attribute("NumericalScheme").empty()) {
		string scheme = num_param.attribute("NumericalScheme").as_string();
		
		if (scheme == "Upwind") {
			numerical_scheme = NumScheme::UPWIND;
		}
		else if (scheme == "MLU") {
			numerical_scheme = NumScheme::MLU;
		}
		else if (scheme == "MUSCL") {
			numerical_scheme = NumScheme::MUSCL;
		}
		else if (scheme == "ENO") {
			numerical_scheme = NumScheme::ENO;
		}
		else {
			throw std::invalid_argument("ERROR: NumericalParameters: wrong NumericalScheme");
		}
	}
	else {
		numerical_scheme = NumScheme::UPWIND;
	}

	//используем или нет коэффициенты (alpha, beta),для задания профиля скорости:
    HeatElement::set_velocity_coeff(isOptionalFlagSwitchedOnInXmlNode("VelocityCoeffOnOff", num_param, true, "ON", "OFF"));

	if (checkOptionalAttributeName("ExchangeArea", num_param)) {
		bool ExchangeArea = isRequiredFlagSwitchedOnInXmlNode("ExchangeArea", num_param, "ON", "OFF");
		HeatElement::set_exchange_area(ExchangeArea);
		FuelRod::ExchangeArea = ExchangeArea;
	}
	else {
		HeatElement::set_exchange_area(false);
		FuelRod::ExchangeArea = true;
	}

	HeatElement::set_heat_problem_search_precision(readOptionalDoubleAttributeFromXmlNode("HeatProblemSearchPrecision",
		                                                                                       num_param,
		                                                                                       1e-7)
	                                               );

	HeatNode::set_temperature_precision(readOptionalDoubleAttributeFromXmlNode("TemperaturePrecision",
		                                           num_param,
		                                           1e-8)
	                                    );

	HeatElement::set_criterion_for_rod_destruction(readOptionalDoubleAttributeFromXmlNode("CriterionForEjectionsAsComponents",
		num_param,
		0.5)
	);


	RadiationExchange::set_turn_on_time(readOptionalDoubleAttributeFromXmlNode("RadiationExchangeTurnOnTime",
		num_param,
		-std::numeric_limits<double>:: max())
	);

	HeatModule::is_output = isOptionalFlagSwitchedOnInXmlNode("WriteOutput", num_param, true);
}

void InputFileReader::readParallel() {
	if (root.child("Parallel").empty()) {
		return;
	}
	pugi::xml_node num_param = root.child("Parallel");
	int threads_number_for_heatElements = 1;
	if (!num_param.attribute("OMP_NTR").empty()) {
		int val = num_param.attribute("OMP_NTR").as_int();
		if (val > 0) {
			threads_number_for_heatElements = val;
		}
		else {
			throw std::invalid_argument("ERROR: Parallel: wrong OMP_NTR");
		}
	}
	
	int threads_number_for_heatProblem = 1;
	if (!num_param.attribute("OMP_NTR_HeatProblem").empty()) {
		int val = num_param.attribute("OMP_NTR_HeatProblem").as_int();
		if (val > 0) {
			threads_number_for_heatProblem = val;
		}
		else {
			throw std::invalid_argument("ERROR: Parallel: wrong OMP_NTR_HeatProblem");
		}
	}
	int available_threads_number = omp_get_num_procs(); //omp_get_max_threads();
	
	if (threads_number_for_heatElements * threads_number_for_heatProblem > available_threads_number) {
		threads_number_for_heatElements = available_threads_number;
		threads_number_for_heatProblem = 1;
		//throw std::invalid_argument("ERROR: Parallel: not enought threads. Max threads number is " + to_string(available_threads_number));
	}
	OMPTreadsNumberHeatElement = threads_number_for_heatElements;
	OMPTreadsNumberHeatProblem = threads_number_for_heatProblem;
}

void InputFileReader::readHeatModuleExtraOptions() {
    pugi::xml_node options = root.child("HeatModuleExtraOptions");
    if (options.empty()) {
        extra_options = nullptr;
        return;
    }
    HeatModule::FuelRod_Canister_on = true;

    auto time_transfer = options.attribute("MeltTransferTime").as_double();
    auto critical_mass = options.attribute("CriticalMassForTransfer").as_double();
    auto canister_radius = options.attribute("CanisterInternalRadius").as_double();
    auto new_mat_name = options.attribute("NewFilmMat").as_string();

    extra_options = make_shared<HeatModuleExtraOptions>(time_transfer, critical_mass, canister_radius, new_mat_name);
}

pugi::xml_node InputFileReader::getRequiredChildInXmlNode(const string &child_name, const pugi::xml_node &node, const string &error_message /* = ""*/) const {
	if (node.child(child_name.c_str()).empty()) {
		string message = error_message;
		if (message.empty()) {
			message = "ERROR: No " + child_name + " tag";
		}
		throw std::invalid_argument(message);
	}

	return node.child(child_name.c_str());
}

void InputFileReader::checkRequiredAttributeName(const string &attribute_name, const pugi::xml_node &node, const string &error_message /* = ""*/) const {
	auto name = attribute_name.c_str();
	if (node.attribute(name).empty()) {
		string message = error_message;
		if (message.empty()) {
			message = "ERROR: No " + attribute_name;
		}
		throw std::invalid_argument(error_message);
	}
}

int InputFileReader::readRequiredIntAttributeFromXmlNode(const string &attribute_name, const pugi::xml_node &node, const string &error_message /*= ""*/) const {
	checkRequiredAttributeName(attribute_name, node, error_message);
	return getFromCFuncOrReadTypeFromXmlNode<int, int>(attribute_name, node, &pugi::xml_attribute::as_int, 0);
}

string InputFileReader::readRequiredStringAttributeFromXmlNode(const string &attribute_name, const pugi::xml_node &node, const string &error_message /*= ""*/) const {
	checkRequiredAttributeName(attribute_name, node, error_message);
	return node.attribute(attribute_name.c_str()).as_string();
}

double InputFileReader::readRequiredDoubleAttributeFromXmlNode(const string &attribute_name, const pugi::xml_node &node, const string &error_message /*= ""*/) const {
	checkRequiredAttributeName(attribute_name, node, error_message);
	return getFromCFuncOrReadTypeFromXmlNode<double, double>(attribute_name, node, &pugi::xml_attribute::as_double, 0);
}

bool InputFileReader::checkOptionalAttributeName(const string &attribute_name, const pugi::xml_node &node) {
	auto name = attribute_name.c_str();
	return !node.attribute(name).empty();
}

std::string InputFileReader::readOptionalStringAttributeFromXmlNode(const string &attribute_name, const pugi::xml_node &node, const string &default_value /*= ""*/) const {
	if (checkOptionalAttributeName(attribute_name, node)) {
		return node.attribute(attribute_name.c_str()).as_string();
	}

	return default_value;
}

double InputFileReader::readOptionalDoubleAttributeFromXmlNode(const string &attribute_name, const pugi::xml_node &node, const double &default_value /*= 0.0*/) const {
	if (checkOptionalAttributeName(attribute_name, node)) {
		return getFromCFuncOrReadTypeFromXmlNode<double, double>(attribute_name, node, &pugi::xml_attribute::as_double, 0);
	}

	return default_value;
}

int InputFileReader::readOptionalIntegerAttributeFromXmlNode(const string &attribute_name, const pugi::xml_node &node, const int &default_value /*= 1.0*/) const {
	if (checkOptionalAttributeName(attribute_name, node)) {
		return getFromCFuncOrReadTypeFromXmlNode<int, int>(attribute_name, node, &pugi::xml_attribute::as_int, 0);
	}

	return default_value;
}

bool InputFileReader::isRequiredFlagSwitchedOnInXmlNode(const string &flag_name, const pugi::xml_node &node, const string &on_value /*= flag_ON*/, const string &off_value /*= flag_OFF*/) const {
	auto flag = readRequiredStringAttributeFromXmlNode(flag_name, node);
	if (flag != on_value && flag != off_value) {
		throw std::invalid_argument("ERROR: " + flag_name + " must be " + on_value + " or " + off_value + ". Is not " + flag);
	}

	return flag == on_value;
}

bool InputFileReader::isOptionalFlagSwitchedOnInXmlNode(const string &flag_name, const pugi::xml_node &node, const bool default_value /*= false*/, const string &on_value /*= flag_ON*/, const string &off_value /*= flag_OFF*/) const {
	if (checkOptionalAttributeName(flag_name, node)) {
		return isRequiredFlagSwitchedOnInXmlNode(flag_name, node, on_value, off_value);
	}

	return default_value;
}

VariableType InputFileReader::VariableTypeForParameterInXmlNode(const string& parameter_name, const pugi::xml_node& node) const {
    string type_param = "Type" + parameter_name;
    VariableType result;
    if (node.attribute(type_param.c_str()).empty()) {
        result = VariableType::Const;
    }
    else {
        string table_flag = node.attribute(type_param.c_str()).as_string();
        if (table_flag == "TF") {
            result = VariableType::Table;
        }
        else if (table_flag == "CF") {
            result = VariableType::ControlFunction;
        }
        else if (table_flag == "Const") {
            result = VariableType::Const;
        }
        else {
            throw std::invalid_argument("ERROR: " + string(node.name()) + ": Invalid argument for " + type_param + ": " + table_flag);
        }
    }

    return result;
}

bool InputFileReader::isInFuelRodListByName(const string &rod_name) {
	for (const auto &rod : fuelRodList) {
		if (rod.first == rod_name)
			return true;
	}

	return false;
}

void InputFileReader::readDissociation() {
    if (root.child("Dissociation").empty()) {
        return;
    }

    pugi::xml_node xml_node_dissociation = root.child("Dissociation");

	atmosphereType = readRequiredStringAttributeFromXmlNode("AtmosphereType", xml_node_dissociation, "ERROR: Dissociation: AtmosphereType is not specified");


	shared_ptr<Convection> convection;
	double velocity_default = 0.;
	double diameter_default = 1e-4;

	pugi::xml_node xml_node_convection = xml_node_dissociation.child("Convection");
	if (xml_node_convection.empty()) {
		convection = make_shared<FreeConvection>(velocity_default, diameter_default, "Free");
	}
	else {
		string conv_type = xml_node_convection.attribute("Type").as_string();

		if (conv_type == "Forced") {
			double conv_velocity = readRequiredDoubleAttributeFromXmlNode("Velocity", xml_node_convection, "ERROR: Dissociation: Velocity is not specified");
			double conv_diameter = readRequiredDoubleAttributeFromXmlNode("Diameter", xml_node_convection, "ERROR: Dissociation: Diameter is not specified");
			convection = make_shared<ForcedConvection>(conv_velocity, conv_diameter, conv_type);
		}
		else {
			double conv_velocity = velocity_default;
			double conv_diameter = readOptionalDoubleAttributeFromXmlNode("Diameter", xml_node_convection, diameter_default);
			convection = make_shared<FreeConvection>(conv_velocity, conv_diameter, conv_type);
		}
	}

	double partial_pressure_U = readOptionalDoubleAttributeFromXmlNode("PartialPressure_U", xml_node_dissociation, 0.);
	double partial_pressure_N2 = readOptionalDoubleAttributeFromXmlNode("PartialPressure_N2", xml_node_dissociation, 0.);
	double partial_pressure_Pu = readOptionalDoubleAttributeFromXmlNode("PartialPressure_Pu", xml_node_dissociation, 0.);

	bool calc_partial_pressure_N2 = isOptionalFlagSwitchedOnInXmlNode("CalculatePartialPressure_N2", xml_node_dissociation, true, "ON", "OFF");
	double partial_pressure_N2_initial = 0.;
	if (calc_partial_pressure_N2) {
		partial_pressure_N2_initial = partial_pressure_N2;
	}

	bool calc_internal_diss = isOptionalFlagSwitchedOnInXmlNode("InternalDissociation", xml_node_dissociation, true, "ON", "OFF");
		
    inputDiss = make_shared<DissociationModel>(
		                                    atmosphereType,
		                                    convection,
		                                    partial_pressure_U,
		                                    partial_pressure_N2,
		                                    partial_pressure_Pu,
		                                    partial_pressure_N2_initial,
		                                    calc_partial_pressure_N2,
		                                    calc_internal_diss
		                                    );


	double factor_U = readOptionalDoubleAttributeFromXmlNode("FactorDiffMassTransCoeff_U", xml_node_dissociation, 1.);
	double factor_N = readOptionalDoubleAttributeFromXmlNode("FactorDiffMassTransCoeff_N", xml_node_dissociation, 1.);
	double factor_condensation = readOptionalDoubleAttributeFromXmlNode("FactorCondensation", xml_node_dissociation, 1.);

    inputDiss->set_mass_transfer_factors(factor_U, factor_N);
    inputDiss->set_condensation_factor(factor_condensation);

    double condensation_coeff_U = readOptionalDoubleAttributeFromXmlNode("CondensationCoeff_U", xml_node_dissociation, 0.59);
    double condensation_coeff_Pu = readOptionalDoubleAttributeFromXmlNode("CondensationCoeff_Pu", xml_node_dissociation, 0.59);
    double condensation_coeff_N = readOptionalDoubleAttributeFromXmlNode("CondensationCoeff_N", xml_node_dissociation, 0.16);
    if (condensation_coeff_U < 0.0 || condensation_coeff_U > 1000.) {
        throw std::invalid_argument("ERROR: Dissociation: CondensationCoeff_U is out of range");
    }
    if (condensation_coeff_N < 0.0 || condensation_coeff_N > 1000.) {
        throw std::invalid_argument("ERROR: Dissociation: condensation_coeff_N is out of range");
    }
    if (condensation_coeff_Pu < 0.0 || condensation_coeff_Pu > 1000.) {
        throw std::invalid_argument("ERROR: Dissociation: condensation_coeff_Pu is out of range");
    }
    inputDiss->set_condensation_coeffs(condensation_coeff_U, condensation_coeff_Pu, condensation_coeff_N);

	const bool fuel_mixing = isOptionalFlagSwitchedOnInXmlNode("FuelMixing", xml_node_dissociation, false, "ON", "OFF");
    inputDiss->set_mix_for_U_Pu(fuel_mixing);

    std::string coolant_diff_option = xml_node_dissociation.attribute("CoolantDiffusionCoeff").as_string("Default");
    if (coolant_diff_option == "Correlation") {
        inputDiss->diffusion_option = DissociationModel::diffusion_in_coolant::Correlation;
    }
    else {
        inputDiss->diffusion_option = DissociationModel::diffusion_in_coolant::Default;
    }
	
	const pugi::xml_node xml_node_external_volume = xml_node_dissociation.child("ExternalVolume");
	if (!xml_node_external_volume.empty()) {
		inputDiss->external_volume = make_shared<ExternalVolume>();
		inputDiss->external_volume->volume = readRequiredDoubleAttributeFromXmlNode("Volume", xml_node_external_volume, "ERROR: Dissociation: ExternalVolume: Volume is not specified");
		inputDiss->external_volume->temperature = readRequiredDoubleAttributeFromXmlNode("Temperature", xml_node_external_volume, "ERROR: Dissociation: ExternalVolume: Temperature is not specified");

		check_range(inputDiss->external_volume->volume, 1e-10, 1e10, "ERROR: Dissociation: ExternalVolume", "Volume");
		check_range(inputDiss->external_volume->temperature, 273., 5000., "ERROR: Dissociation: ExternalVolume", "Temperature");
	}

	bool dissociation_through_melt = isOptionalFlagSwitchedOnInXmlNode("DissociationThroughMelt", xml_node_dissociation, true, "ON", "OFF");
	inputDiss->dissociation_through_melt = dissociation_through_melt;
}

void InputFileReader::read_UN_Pb_Steel_Ar_O2_interaction() {
	pugi::xml_node UN_O_InteractionNode = root.child("UN_O_Interaction");

	if (UN_O_InteractionNode.empty()) {
		return;
	}

	un_o_model = make_shared<UN_O_Interaction::UN_O_InteractionModel>();


	un_o_model->name = readRequiredStringAttributeFromXmlNode("Name", UN_O_InteractionNode, "ERROR: UN_O_Interaction: Name is required");

	un_o_model->melt_diffusion_model = readRequiredIntAttributeFromXmlNode("MeltDiffusionModel", UN_O_InteractionNode, "ERROR: UN_O_Interaction: MeltDiffusionModel is required");

	un_o_model->is_multiplier_solid_diffusion = checkOptionalAttributeName("MultiplierSolidDiffusion", UN_O_InteractionNode);
	if (un_o_model->is_multiplier_solid_diffusion) {
		un_o_model->multiplier_solid_diffusion = readRequiredDoubleAttributeFromXmlNode("MultiplierSolidDiffusion", UN_O_InteractionNode);
	}

    un_o_model->is_multiplier_liquid_diffusion = checkOptionalAttributeName("MultiplierLiquidDiffusion", UN_O_InteractionNode);
	if (un_o_model->is_multiplier_liquid_diffusion) {
		un_o_model->multiplier_liquid_diffusion = readRequiredDoubleAttributeFromXmlNode("MultiplierLiquidDiffusion", UN_O_InteractionNode);
	}

	un_o_model->is_simplified_melt_composition = isOptionalFlagSwitchedOnInXmlNode("SimplifiedMeltComposition", UN_O_InteractionNode, true, "ON", "OFF");
	if (un_o_model->melt_diffusion_model == 2) {
		un_o_model->is_simplified_melt_composition = false;
	}

	//-----------------------------------------------------------------------
	// Geometry of the system
	//-----------------------------------------------------------------------
	double area_surf = readRequiredDoubleAttributeFromXmlNode("SurfaceArea", UN_O_InteractionNode, "ERROR: UN_O_Interaction: SurfaceArea is required");
	double height_pellet = readRequiredDoubleAttributeFromXmlNode("PelletHeight", UN_O_InteractionNode, "ERROR: UN_O_Interaction: PelletHeight is required");
	double height_melt = readRequiredDoubleAttributeFromXmlNode("MeltHeight", UN_O_InteractionNode, "ERROR: UN_O_Interaction: MeltHeight is required");
	
	un_o_model->area_surf = area_surf;
	un_o_model->fuel_and_melt_param.height_pellet = height_pellet;
	un_o_model->fuel_and_melt_param.volume_pellet = area_surf * height_pellet;
	un_o_model->fuel_and_melt_param.thick_melt = height_melt;
	un_o_model->fuel_and_melt_param.thick_melt_init = height_melt;
	un_o_model->fuel_and_melt_param.volume_up_melt = area_surf * height_melt;


	un_o_model->nz_melt = readOptionalIntegerAttributeFromXmlNode("MeltDiscretization", UN_O_InteractionNode, 10);
	un_o_model->nz_melt1 = readOptionalIntegerAttributeFromXmlNode("LeadDiscretization", UN_O_InteractionNode, 10);
	un_o_model->nz_pell = readOptionalIntegerAttributeFromXmlNode("PellDiscretization", UN_O_InteractionNode, 10);

	un_o_model->nz_max = max(un_o_model->nz_melt, un_o_model->nz_pell) + 2;
	if (un_o_model->melt_diffusion_model >= 2) {
		un_o_model->nz_max = max(un_o_model->nz_melt + un_o_model->nz_melt1, un_o_model->nz_pell) + 2;
	}

	un_o_model->is_model_hydraulic_diameter = isOptionalFlagSwitchedOnInXmlNode("HydraulicDiameterModel", UN_O_InteractionNode, false, "ON", "OFF");
	if (!un_o_model->is_model_hydraulic_diameter) {
	    un_o_model->hydraulic_diam = readOptionalDoubleAttributeFromXmlNode("HydraulicDiameter", UN_O_InteractionNode, 1e-2);
	}

	un_o_model->temperature_gas_out = readRequiredDoubleAttributeFromXmlNode("TempBulkGas", UN_O_InteractionNode, "ERROR: UN_O_Interaction: TempBulkGas is required");
	un_o_model->Nusselt = readRequiredDoubleAttributeFromXmlNode("Nusselt", UN_O_InteractionNode, "ERROR: UN_O_Interaction: Nusselt is required");

	// поля для связки с тепловой структурой, для отдельного расчёта в модели не используются:
	un_o_model->high_Pb = readOptionalDoubleAttributeFromXmlNode("HighPb", UN_O_InteractionNode, UNDEF_DOUBLE);
	un_o_model->mass_Pb = readOptionalDoubleAttributeFromXmlNode("MassPb", UN_O_InteractionNode, UNDEF_DOUBLE);
	un_o_model->temperature_Pb = readOptionalDoubleAttributeFromXmlNode("TemperaturePb", UN_O_InteractionNode, UNDEF_DOUBLE);
	un_o_model->Fe_concentration = readOptionalDoubleAttributeFromXmlNode("ConcentrationFe", UN_O_InteractionNode, UNDEF_DOUBLE);
	un_o_model->Cr_concentration = readOptionalDoubleAttributeFromXmlNode("ConcentrationCr", UN_O_InteractionNode, UNDEF_DOUBLE);

	string error_message = "ERROR: UN_O_Interaction: " + un_o_model->name + ": HighPb is out of range";
    checkOptionalAttributeRange("HighPb", UN_O_InteractionNode, un_o_model->high_Pb, 1e-6, 1., error_message);
	error_message = "ERROR: UN_O_Interaction: " + un_o_model->name + ": MassPb is out of range";
	checkOptionalAttributeRange("MassPb", UN_O_InteractionNode, un_o_model->mass_Pb, 1e-10, 1e10, error_message);
	error_message = "ERROR: UN_O_Interaction: " + un_o_model->name + ": TemperaturePb is out of range";
	checkOptionalAttributeRange("TemperaturePb", UN_O_InteractionNode, un_o_model->temperature_Pb, 200., 5000., error_message);
	error_message = "ERROR: UN_O_Interaction: " + un_o_model->name + ": ConcentrationFe is out of range";
	checkOptionalAttributeRange("ConcentrationFe", UN_O_InteractionNode, un_o_model->Fe_concentration, 0.1, 1., error_message);
	error_message = "ERROR: UN_O_Interaction: " + un_o_model->name + ": ConcentrationCr is out of range";
	checkOptionalAttributeRange("ConcentrationCr", UN_O_InteractionNode, un_o_model->Cr_concentration, 0., 0.9, error_message);

	// пока тут, чтобы нужные массивы инициализировались
	un_o_model->initialize_chemical_composition_and_properties();

    pugi::xml_node melt_composition = UN_O_InteractionNode.child("MeltComposition");
	if (melt_composition.empty()) {
		throw std::invalid_argument("ERROR: UN_O_Interaction: MeltComposition is required");
	}
	
	struct MeltComposition { // таблица MeltComposition
		string name;
		double mass;
	};

	vector<MeltComposition> input_melt_composition;
	for (pugi::xml_node material_node = melt_composition.child("Material"); material_node; material_node = material_node.next_sibling("Material")) {
		string material_name = readRequiredStringAttributeFromXmlNode("Name", material_node, "ERROR: UN_O_Interaction: MeltComposition: Material: Name is required");
		double mass = readRequiredDoubleAttributeFromXmlNode("Mass", material_node, "ERROR: UN_O_Interaction: MeltComposition: Material " + material_name + ": Mass is required");

		input_melt_composition.push_back({ material_name, mass });
	}

	if (input_melt_composition.empty()) {
		throw std::invalid_argument("ERROR: UN_O_Interaction: MeltComposition is empty");
	}

	// пока тут, потом перенесем
	un_o_model->input_mass.resize(un_o_model->n_species_max, 0.);

	size_t nspecIN = input_melt_composition.size(); // размерность таблицы <MeltComposition>
	vector<size_t> nspec_in(nspecIN, 0);
	size_t n_spec_curr = 0;
	for (size_t j = 0; j < nspecIN; j++) {
		string in_name = input_melt_composition[j].name;
		double in_mass = input_melt_composition[j].mass;
		for (size_t i = 0; i < un_o_model->n_species; i++) {
			size_t name_len = un_o_model->species_name[i].length();
			string mod_name = un_o_model->species_name[i].substr(0, name_len - 1);

			if (in_name == mod_name || in_name == un_o_model->species_name[i]) {
				un_o_model->input_mass[i] = in_mass;
				if (un_o_model->input_mass[i] > 0.) {
					nspec_in[n_spec_curr] = i;
					un_o_model->species_list[n_spec_curr] = i;
					n_spec_curr++;
					break;
				}
			}
		}
	}
	un_o_model->n_species_act = n_spec_curr;

	un_o_model->redefine_melt_thickness_using_initial_masses();


	readPropertyFromXmlNode<UN_O_Interaction::UN_O_InteractionModel>(UN_O_InteractionNode
												                    , un_o_model.get()
												                    , "Temperature"
												                    , &UN_O_Interaction::UN_O_InteractionModel::set_temperature_current
												                    , &UN_O_Interaction::UN_O_InteractionModel::set_type_temperature_current
											                        , &UN_O_Interaction::UN_O_InteractionModel::set_table_temperature_current
									                                , &UN_O_Interaction::UN_O_InteractionModel::set_cf_name_temperature_current
												                    , "ERROR: UN_O_Interaction: No attribute Temperature"
											                        );

	readPropertyFromXmlNode<UN_O_Interaction::UN_O_InteractionModel>(UN_O_InteractionNode
												                    , un_o_model.get()
												                    , "FlowRateGas"
												                    , &UN_O_Interaction::UN_O_InteractionModel::set_flow_rate_gas
												                    , &UN_O_Interaction::UN_O_InteractionModel::set_type_flow_rate_gas
											                        , &UN_O_Interaction::UN_O_InteractionModel::set_table_flow_rate_gas
									                                , &UN_O_Interaction::UN_O_InteractionModel::set_cf_name_flow_rate_gas
												                    , "ERROR: UN_O_Interaction: No attribute FlowRateGas"
											                        );
	// transform flow rate in [m^3/s] for standard conditions to [moles/s] (STP: P = Patm0(1 atm) and T = 298.15 K)
	un_o_model->flow_rate_gas *= prt0;

	readPropertyFromXmlNode<UN_O_Interaction::UN_O_InteractionModel>(UN_O_InteractionNode
												                    , un_o_model.get()
												                    , "PressureGas"
												                    , &UN_O_Interaction::UN_O_InteractionModel::set_pressure_total
												                    , &UN_O_Interaction::UN_O_InteractionModel::set_type_pressure_total
											                        , &UN_O_Interaction::UN_O_InteractionModel::set_table_pressure_total
									                                , &UN_O_Interaction::UN_O_InteractionModel::set_cf_name_pressure_total
												                    , "ERROR: UN_O_Interaction: No attribute PressureGas"
											                        );

	readPropertyFromXmlNode<UN_O_Interaction::UN_O_InteractionModel>(UN_O_InteractionNode
												                    , un_o_model.get()
												                    , "FractionGasH2O"
												                    , &UN_O_Interaction::UN_O_InteractionModel::set_fraction_gas_H2O
												                    , &UN_O_Interaction::UN_O_InteractionModel::set_type_fraction_gas_H2O
											                        , &UN_O_Interaction::UN_O_InteractionModel::set_table_fraction_gas_H2O
									                                , &UN_O_Interaction::UN_O_InteractionModel::set_cf_name_fraction_gas_H2O
												                    , "ERROR: UN_O_Interaction: No attribute FractionGasH2O"
											                        );

	readPropertyFromXmlNode<UN_O_Interaction::UN_O_InteractionModel>(UN_O_InteractionNode
												                    , un_o_model.get()
												                    , "FractionGasH2"
												                    , &UN_O_Interaction::UN_O_InteractionModel::set_fraction_gas_H2
												                    , &UN_O_Interaction::UN_O_InteractionModel::set_type_fraction_gas_H2
											                        , &UN_O_Interaction::UN_O_InteractionModel::set_table_fraction_gas_H2
									                                , &UN_O_Interaction::UN_O_InteractionModel::set_cf_name_fraction_gas_H2
												                    , "ERROR: UN_O_Interaction: No attribute FractionGasH2"
											                        );

	readPropertyFromXmlNode<UN_O_Interaction::UN_O_InteractionModel>(UN_O_InteractionNode
												                    , un_o_model.get()
												                    , "FractionGasO2"
												                    , &UN_O_Interaction::UN_O_InteractionModel::set_fraction_gas_O2
												                    , &UN_O_Interaction::UN_O_InteractionModel::set_type_fraction_gas_O2
											                        , &UN_O_Interaction::UN_O_InteractionModel::set_table_fraction_gas_O2
									                                , &UN_O_Interaction::UN_O_InteractionModel::set_cf_name_fraction_gas_O2
												                    , "ERROR: UN_O_Interaction: No attribute FractionGasO2"
											                        );

	

}

bool InputFileReader::readNuclides() {
	pugi::xml_node nuclidesNode = root.child("Nuclides");
	
	if (nuclidesNode.empty()) {
        return false;
    }

	string nuclidesNames = readRequiredStringAttributeFromXmlNode("NuclideNames", nuclidesNode, "ERROR: No NuclideNames for Nuclides.");
	vector<string> vectorNuclideNames;
	SplitStringIntoString(nuclidesNames, vectorNuclideNames);

	for (pugi::xml_node nuclideMaterialNode = nuclidesNode.child("Material"); nuclideMaterialNode; nuclideMaterialNode = nuclideMaterialNode.next_sibling("Material")) {
		string materialName = readRequiredStringAttributeFromXmlNode("Name", nuclideMaterialNode, "ERROR: No Name in Material for Nuclides.");
		string concentrations = readRequiredStringAttributeFromXmlNode("Concentrations", nuclideMaterialNode, "ERROR: No Concentrations in Material for Nuclides.");

		vector<double> vectorConcentrations;
		SplitStringIntoDouble(concentrations, vectorConcentrations);
		
		unordered_map<string, double> nuclideNameConcentrationMap; // key - nuclideName, value - concentration
		assert(vectorNuclideNames.size() == vectorConcentrations.size());
		for (size_t i = 0; i < vectorNuclideNames.size(); ++i) {
			nuclideNameConcentrationMap[vectorNuclideNames[i]] = vectorConcentrations[i];
		}

		inputNuclides[materialName] = nuclideNameConcentrationMap;
	}

	return true;
}

void InputFileReader::readSecondaryCriticality() {
	pugi::xml_node options = root.child("SecondaryCriticality");
	if (options.empty()) {
		secondary_criticality = nullptr;
		return;
	}

	auto solving_time = options.attribute("SolvingTime").as_double();
	string heat_struct_name = options.attribute("Name").as_string();
	string atm_name = options.attribute("AtmosphereStateName").as_string();

	secondary_criticality = make_shared<SecondaryCriticality>(heat_struct_name, atm_name, solving_time);
}
#ifdef _WIN32
void InputFileReader::read_UO2_Dissolution() {
    pugi::xml_node UO2_dissolution = root.child("Dissolution");

    if (UO2_dissolution.empty()) {
        return;
    }

    uo2_dissolution = std::make_shared<Dissolution>();
    uo2_dissolution->Experiment = readOptionalIntegerAttributeFromXmlNode("Experiment", UO2_dissolution, 0);
    uo2_dissolution->Approximation = readOptionalIntegerAttributeFromXmlNode("Approximation", UO2_dissolution, 1);
    uo2_dissolution->KindApproximation = readOptionalIntegerAttributeFromXmlNode("KindApproximation", UO2_dissolution, 0);

    uo2_dissolution->V0Crucible = readOptionalDoubleAttributeFromXmlNode("V0Crucible", UO2_dissolution, 1.018e-7);
    uo2_dissolution->R0Crucible = readOptionalDoubleAttributeFromXmlNode("R0Crucible", UO2_dissolution, 2.45e-3);
    uo2_dissolution->hCrucible = readOptionalDoubleAttributeFromXmlNode("hCrucible", UO2_dissolution, 2.75e-3);
    uo2_dissolution->TempCrucible = readOptionalDoubleAttributeFromXmlNode("TempCrucible", UO2_dissolution, 2473);

    uo2_dissolution->TempFR = readOptionalDoubleAttributeFromXmlNode("TempFR", UO2_dissolution, 2429.);
    uo2_dissolution->V0FR = readOptionalDoubleAttributeFromXmlNode("V0FR", UO2_dissolution, 3.929e-6);
    uo2_dissolution->Vex0FR = readOptionalDoubleAttributeFromXmlNode("Vex0FR", UO2_dissolution, 9.892e-7);
    uo2_dissolution->Vin0FR = readOptionalDoubleAttributeFromXmlNode("Vin0FR", UO2_dissolution, 7.768e-6);
    uo2_dissolution->Sex0FR = readOptionalDoubleAttributeFromXmlNode("Sex0FR", UO2_dissolution, 5.347e-4);
    uo2_dissolution->Sin0FR = readOptionalDoubleAttributeFromXmlNode("Sin0FR", UO2_dissolution, 1.192e-3);
    uo2_dissolution->hexFR = readOptionalDoubleAttributeFromXmlNode("hexFR", UO2_dissolution, 3.7e-3);
    uo2_dissolution->hinFR = readOptionalDoubleAttributeFromXmlNode("hinFR", UO2_dissolution, 5.0e-3);

    uo2_dissolution->kappa0FR = 1.e-4;
    uo2_dissolution->epsFR = 0.99;
    uo2_dissolution->xFR = 0.05;
    uo2_dissolution->zFR = 0.06;
}
#endif

void InputFileReader::readEutectics() {
	std::string simple_model_name = "SimpleModel";
	std::string diffusion_model_name = "DiffusionModel";
	std::string diffusion_un_model_name = "DiffusionUNModel";
    for (pugi::xml_node input_eutectic = root.child("Eutectic"); input_eutectic; input_eutectic = input_eutectic.next_sibling("Eutectic")) {
        std::string input_name = readRequiredStringAttributeFromXmlNode("Name", input_eutectic, "ERROR: No name of Eutectic");
        std::string input_model = readRequiredStringAttributeFromXmlNode("EutecticModel", input_eutectic, "ERROR: No name of EutecticModel");

        std::string material_for_eutectic_1 = readRequiredStringAttributeFromXmlNode("MaterialForEutecticInteraction_1", input_eutectic, "ERROR: No material for eutectic interaction in Eutectic");
        std::string material_for_eutectic_2 = readRequiredStringAttributeFromXmlNode("MaterialForEutecticInteraction_2", input_eutectic, "ERROR: No material for eutectic interaction in Eutectic");
        std::string material_for_eutectic_result = readRequiredStringAttributeFromXmlNode("MaterialOfEutectic", input_eutectic, "ERROR: No material of eutectic in Eutectic");

        std::string struct_name = "";
        if (material_for_eutectic_1 == "Uranium" || material_for_eutectic_1 == "UN") {
            struct_name = material_for_eutectic_2;
        }
        else if (material_for_eutectic_2 == "Uranium" || material_for_eutectic_2 == "UN") {
            struct_name = material_for_eutectic_1;
        }
        else {
            throw std::invalid_argument("ERROR: readEutectics: MaterialForEutecticInteraction is only \"Uranium\" or \"UN\" possible");
        }
		// это все стали и материалы, которы есть в БД и для которых возможно эвтектическое взаимодействие
		/**/
        std::vector<std::string> possible_simple_names = { "Mo", "CM2A",
                                                    "Fe", "St316", "EK164", "St45", "22K", "SS304",
                                                    "10GH2MFA",
                                                    "12X18H9", "12X18H9T", "12X18H10T", "12X13",
                                                    "15GH2MFA", "15X2HMFA","15GH2MFA-A", "15X2MFA", "15X3HMFA",
                                                    "X18H12",
                                                    "20X13", "X20H80",
                                                    "25X3MFA",
                                                    "XH77TUR",
                                                    "08X16H11M3", "08X18H10T", "08X13",
                                                    "EP850", "EP450", "EP450DUO", "EP823",
                                                    "EI847",
                                                    "CHS68" };
		

		// пока оставляем только стали, для которых известны скорости растворения из экспериментов(EP823, EP450, EP450DUO, EK164)
		// еще оставляю Fe и Mo, CM2A.
        std::vector<std::string> possible_diffusion_names = { "Mo", "CM2A",
                                                    "Fe", "EK164", "EP450", "EP450DUO", "EP823"};
		std::vector<std::string> possible_diffusion_un_names = { "Fe", "EK164"};

        std::vector<std::string> check_possible_mat;
        if (input_model == simple_model_name) {
            check_possible_mat = std::move(possible_simple_names);
        }
        else if (input_model == diffusion_model_name) {
            check_possible_mat = std::move(possible_diffusion_names);
        }
		else if (input_model == diffusion_un_model_name) {
			check_possible_mat = std::move(possible_diffusion_un_names);
		}

        if (!check_possible_mat.empty()) {
            auto name_it = std::find(check_possible_mat.begin(), check_possible_mat.end(), struct_name);
            if (name_it == check_possible_mat.end()) { // в этом случае имя отсутсвует в списке
                std::string error_string = "ERROR: readEutectics: MaterialForEutecticInteraction is only ";
                error_string += std::accumulate(check_possible_mat.begin(), check_possible_mat.end(), std::string(),
                    [](const std::string& a, const std::string& b) { return a.empty() ? b : a + ", " + b;
                    });
                error_string += " possible";

                throw std::invalid_argument(error_string);
            }
        }

        std::string direction_type = readOptionalStringAttributeFromXmlNode("DirectionType", input_eutectic, "Both");
        if (direction_type != "Radial" && direction_type != "Axial" && direction_type != "Both") {
            throw std::invalid_argument("ERROR: readEutectics: EutecticModel: DirectionType is only \"Radial\", \"Axial\" or \"Both\" possible");
        }

        std::shared_ptr<EutecticBase> eutectic = nullptr;
        if (input_model == simple_model_name) {
            eutectic = std::make_shared<SimpleEutecticModel>(material_for_eutectic_result, material_for_eutectic_1, material_for_eutectic_2);
        }
        else if (input_model == diffusion_model_name) {
			bool analitical_flag = isOptionalFlagSwitchedOnInXmlNode("AnaliticalSolution", input_eutectic, false, "ON", "OFF");
			int nz = readRequiredIntAttributeFromXmlNode("NZ", input_eutectic, "ERROR: No NZ in Eutectic " + input_name);
			double diff_factor = readOptionalDoubleAttributeFromXmlNode("DiffusionFactor", input_eutectic, 1.0);
			double rate_const = readOptionalDoubleAttributeFromXmlNode("RateConst", input_eutectic, 1.0);

            string error_message = "ERROR: readEutectics: EutecticModel: NZ is out of range";
            checkOptionalAttributeRange("NZ", input_eutectic, nz, 3, 50000, error_message);
            error_message = "ERROR: readEutectics: EutecticModel: DiffusionFactor is out of range";
            checkOptionalAttributeRange("DiffusionFactor", input_eutectic, diff_factor, 0.0, 1.e5, error_message);
			error_message = "ERROR: readEutectics: EutecticModel: RateConst is out of range";
			checkOptionalAttributeRange("RateConst", input_eutectic, rate_const, 0.0, 1.e5, error_message);

            eutectic = std::make_shared<DiffusionEutecticModel>(material_for_eutectic_result, material_for_eutectic_1, material_for_eutectic_2,
                analitical_flag, nz, diff_factor, rate_const);
        }
		else if (input_model == diffusion_un_model_name) {
			bool analitical_flag = isOptionalFlagSwitchedOnInXmlNode("AnaliticalSolution", input_eutectic, false, "ON", "OFF");
			int nz = readRequiredIntAttributeFromXmlNode("NZ", input_eutectic, "ERROR: No NZ in Eutectic " + input_name);
			double diff_factor = readOptionalDoubleAttributeFromXmlNode("DiffusionFactor", input_eutectic, 1.0);
			double un_activity = readOptionalDoubleAttributeFromXmlNode("UNActivity", input_eutectic, 1.0);
			double rate_const = readOptionalDoubleAttributeFromXmlNode("RateConst", input_eutectic, 1.0);

			string error_message = "ERROR: readEutectics: EutecticModel: NZ is out of range";
			checkOptionalAttributeRange("NZ", input_eutectic, nz, 3, 50000, error_message);
			error_message = "ERROR: readEutectics: EutecticModel: DiffusionFactor is out of range";
			checkOptionalAttributeRange("DiffusionFactor", input_eutectic, diff_factor, 0.0, 1.e5, error_message);
			error_message = "ERROR: readEutectics: EutecticModel: UNActivity is out of range";
			checkOptionalAttributeRange("UNActivity", input_eutectic, un_activity, 0.0, 1.0e5, error_message);
			error_message = "ERROR: readEutectics: EutecticModel: RateConst is out of range";
			checkOptionalAttributeRange("RateConst", input_eutectic, rate_const, 0.0, 1.e5, error_message);

			eutectic = std::make_shared<DiffusionUNEutecticModel>(material_for_eutectic_result, material_for_eutectic_1, material_for_eutectic_2, analitical_flag, nz, diff_factor, un_activity);
		}
        else {
            throw std::invalid_argument("ERROR: readEutectics: EutecticModel is empty");
        }

        if (direction_type == "Radial") {
            eutectic->direction = EutecticBase::Model_direction::Radial_direction;
        }
        else if (direction_type == "Axial") {
            eutectic->direction = EutecticBase::Model_direction::Axial_direction;
        }
        else if (direction_type == "Both") {
            eutectic->direction = EutecticBase::Model_direction::Both_direction;
        }

        eutectic->name = input_name;

        eutectic_vector.push_back(eutectic);
    }
}

/*
 void InputFileReader::read_UN_Pb_Steel_Ar_O2_interaction() {
	if (root.child("UN_Pb_Steel_Ar_O2_interaction").empty()) {
		return;
	}

	pugi::xml_node xml_node_interaction = root.child("UN_Pb_Steel_Ar_O2_interaction");

	// read gas layer state
	string base_gas_name = readRequiredStringAttributeFromXmlNode("BaseGas", xml_node_interaction, "ERROR: UN_Pb_Steel_Ar_O2_interaction: BaseGas is not specified");
	string all_gas_material_names = readRequiredStringAttributeFromXmlNode("Gas", xml_node_interaction, "");
	double total_press_gas = readRequiredDoubleAttributeFromXmlNode("PressureGas", xml_node_interaction, "ERROR: No PressureGas in UN_Pb_Steel_Ar_O2_interaction");
	double temp_volume_gas = readRequiredDoubleAttributeFromXmlNode("TemperatureGas", xml_node_interaction, "ERROR: No TemperatureGas in UN_Pb_Steel_Ar_O2_interaction");

	// удаление пробелов:
	all_gas_material_names.erase(
			                     remove_if(all_gas_material_names.begin()
								                , all_gas_material_names.end()
                                                , [](const char& ch) { return isspace(ch); }
								                )
			                    , all_gas_material_names.end()
		                        );

	vector<string> gas_material_names = STRING_SPLIT(all_gas_material_names, ",");
	if (gas_material_names.empty()) {
		throw std::invalid_argument("ERROR: No correct Gases in UN_Pb_Steel_Ar_O2_interaction");
	}

	vector<shared_ptr<StructuralMaterial>> gas_materials;
	// проверяем есть ли материалы в базе
	for (const auto &mat_name : gas_material_names) {
		auto it = inputMaterials.find(mat_name);
		gas_materials.push_back(it->second->ptr_copy());
		if (it == inputMaterials.end()) {
			throw std::invalid_argument("ERROR: Gas " + mat_name + " for UN_Pb_Steel_Ar_O2_interaction not found");
		}
	}
	
	// проверка, что есть O2
	// БИНАРНЫЕ КОЭФФИЦИЕНТЫ, КАКИЕ НУЖНЫ?








	atmosphereType = readRequiredStringAttributeFromXmlNode("AtmosphereType", xml_node_dissociation, "ERROR: Dissociation: AtmosphereType is not specified");


	shared_ptr<Convection> convection;
	double velocity_default = 0.;
	double diameter_default = 1e-4;

	pugi::xml_node xml_node_convection = xml_node_dissociation.child("Convection");
	if (xml_node_convection.empty()) {
		convection = make_shared<FreeConvection>(velocity_default, diameter_default, "Free");
	}
	else {
		string conv_type = xml_node_convection.attribute("Type").as_string();

		if (conv_type == "Forced") {
			double conv_velocity = readRequiredDoubleAttributeFromXmlNode("Velocity", xml_node_convection, "ERROR: Dissociation: Velocity is not specified");
			double conv_diameter = readRequiredDoubleAttributeFromXmlNode("Diameter", xml_node_convection, "ERROR: Dissociation: Diameter is not specified");
			convection = make_shared<ForcedConvection>(conv_velocity, conv_diameter, conv_type);
		}
		else {
			double conv_velocity = velocity_default;
			double conv_diameter = readOptionalDoubleAttributeFromXmlNode("Diameter", xml_node_convection, diameter_default);
			convection = make_shared<FreeConvection>(conv_velocity, conv_diameter, conv_type);
		}
	}

	double partial_pressure_U = readOptionalDoubleAttributeFromXmlNode("PartialPressure_U", xml_node_dissociation, 0.);
	double partial_pressure_N2 = readOptionalDoubleAttributeFromXmlNode("PartialPressure_N2", xml_node_dissociation, 0.);
	double partial_pressure_Pu = readOptionalDoubleAttributeFromXmlNode("PartialPressure_Pu", xml_node_dissociation, 0.);
	bool calc_partial_pressure_N2 = isOptionalFlagSwitchedOnInXmlNode("CalculatePartialPressure_N2", xml_node_dissociation, true, "ON", "OFF");

	inputDiss = make_shared<DissociationModel>(atmosphereType, convection, partial_pressure_U, partial_pressure_N2, partial_pressure_Pu, calc_partial_pressure_N2);


	double factor_U = readOptionalDoubleAttributeFromXmlNode("FactorDiffMassTransCoeff_U", xml_node_dissociation, 1.);
	double factor_N = readOptionalDoubleAttributeFromXmlNode("FactorDiffMassTransCoeff_N", xml_node_dissociation, 1.);
	double factor_condensation = readOptionalDoubleAttributeFromXmlNode("FactorCondensation", xml_node_dissociation, 1.);

	inputDiss->set_mass_transfer_factors(factor_U, factor_N);
	inputDiss->set_condensation_factor(factor_condensation);

	bool fuel_mixing = isOptionalFlagSwitchedOnInXmlNode("FuelMixing", xml_node_dissociation, false, "ON", "OFF");
	inputDiss->set_mix_for_U_Pu(fuel_mixing);
}
*/