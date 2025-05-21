#pragma once
#include <string>
#include <iostream>
#include <vector>
#include <map>
#include <memory>

#include "pugixml.hpp"

#include "../HeatElement/HeatElement.h"
#include "../HeatBound/HeatBound.h"
#include "../HeatBound/HeatBoundCell.h"
#include "../Materials/InputMaterial.h"
#include "../Materials/FissionProduct.h"
#include "../Materials/MaterialsCollection.h"
#include "../HeatElement/HeatStruct.h"
#include "../Cell/CylindricalMaterialCell.h"
#include "../Cell/Section.h"
#include "../Film/FilmBound.h"
#include "../Time/timeController.h"
#include "../../SAFR_DLL/Core/Core.h"
#include "../RadiationExchange/RadiationExchangeViewFactor.h"
#include "../RadiationExchange/RadiationExchange.h"
#include "../MeltPool/MeltPool.h"
#include "../MeltPool/MeltPoolLayer.h"
#include "../Dissociation/DissociationModel.h"
#include "../SAFR_DLL/Core/Canister.h"
#include "InputTable.h"
#include "Connection.h"
#include "MultiTable.h"
#include "../../SAFR_DLL/headers/smart3/cfunc.h"
#include "../HeatElement/HeatStruct_interTransfer.h"
#include "../UN_O_Interaction/UN_O_InteractionModel.h"
#include "../UN_O_Interaction/UN_O_InteractionModel_HeatStruct_Link.h"
#include "../../SAFR_DLL/headers/dissolution.h"
#include "../HeatElement/HeatStruct_changeFilmFlowability.h"

extern ControlFunction cfunc;

class HeatModuleExtraOptions;
class SecondaryCriticality;

enum typeRadialMesh {
	uniRad,	// равномерная по радиусу сетка (в пределах одного материала)
	uniVol,	// равномерная по объёму радиальных слоёв (в пределах одного материала)
	undef
};

class BerkutBatch {
public:
	BerkutBatch(std::string& batch_name_, std::string& fuel_name_, double rel_dens_, double pu_part_):
	batch_name(batch_name_),
	fuel_name(fuel_name_),
	rel_dens(rel_dens_),
	pu_part(pu_part_) {}

	std::string batch_name;
	std::string fuel_name;
	double rel_dens;
	double pu_part;
};

class MaterialsParam {
public:
    MaterialsParam(std::string& name, double rel_dens_, double pu_part_, double burn_up_, double gas_pressure_) :
        mat_name(name),
        rel_dens(rel_dens_),
        pu_part(pu_part_),
        burn_up(burn_up_),
        gas_pressure(gas_pressure_)
    {}

	std::string mat_name;
	double rel_dens;
	double pu_part;
	double burn_up;
	double gas_pressure;
};

class BerkutParam{
	public:
        BerkutParam(const double& tSC, const double& tInit, const double& rDens, const double& pu,
            const double& gPressure, const string& fuelMat, const string& cladMat,
            const string& gas, const string& heatTable, const bool use,
            const typeRadialMesh mType, const size_t Nr_h, const size_t Nr_p, const size_t Nr_g,
            const size_t Nr_c,
            std::vector<std::shared_ptr<BerkutBatch>>& input_batches) :
            tempSC(tSC)
            , tempInit(tInit)
            , relDens(rDens)
            , puFraction(pu)
            , gasPressure(gPressure)
            , fuelMaterial(fuelMat)
            , claddingMaterial(cladMat)
            , gasName(gas)
            , moduleUse(use)
            , tableHeatPower(heatTable)
            , meshType(mType)
            , Nr_hole(Nr_h)
            , Nr_pellet(Nr_p)
            , Nr_gap(Nr_g)
            , Nr_cladding(Nr_c)
            , berkut_batches(input_batches)
        {}

		~BerkutParam(){};

		double tempSC; // температура в стандартных условиях (при которых измеряются геометрические размеры твэлов)
		double tempInit; // начальная температура
		double relDens; // относительная плотность топлива, равная отношению плотности изготовления к теоретической плотности материала
		double puFraction; // массовая доля оксида или нитрида плутония
		double gasPressure; // давление газа закачки
		string fuelMaterial; // тип топлива
		string claddingMaterial; // материал оболочки
		string gasName; // газ закачки
		string tableHeatPower; // таблица для энерговыделения
		bool moduleUse; // флаг включения модуля
		typeRadialMesh meshType;
		size_t Nr_hole; // число радиальных ячеек в центральном отверстии
		size_t Nr_pellet; // число радиальных ячеек в топливной таблетке
		size_t Nr_gap; // число радиальных ячеек в газовом зазоре
		size_t Nr_cladding; // число радиальных ячеек в оболочке
	    std::vector<std::shared_ptr<BerkutBatch>> berkut_batches;
};

/**!
@brief Парсер входного файла
@details Считывает входной файл и создает тепловые элементы с заданными характеристиками
@date 22 Jan 2015
@authors Kuznetsova Marina, Anton Butov
@bug Обработка значений, заданных с помощью констант
*/

class InputFileReader {
    public:
		//static InputFileReader& getReader(const std::string inputFileName = "");

		//static void deleteReader();

	    /**
	    @brief Основной метод класса - считывание входного файла.
	    @details Во входном файле считывает тэги:
	    <HeatStruct>, <Constant>, <HeatBound>, <Connection>, <MatProp>
	    Если считывание прошло успешно, возвращает true.
	    Иначе возвращает false, сообщение об ошибке может быть получено вызовом getErrorMessage()
	    */
	    bool readData(const pugi::xml_node &task, const string& module_name);

	    /**
	    @brief Вспомогательный метод для readData
	    @details
	    */
	    bool readRoot(const string& module_name);

	    /**
	    @brief Вспомогательный метод для readData
	    @details Считывание теловых структур в формате ввода модуля БЕРКУТ
	    */
	    bool readRootInBERKUTFormat(const map<string, string> &fuelRodList);

	    /**
	    @brief Возвращает сообщение об ошибке
	    */
	    std::string getErrorMessage()const;


	    /**
	    @brief Возвращает массив указателей на тепловые элементы, заданные во входном файле.
	    */
	    std::vector <shared_ptr<HeatElement>> getHeatElements();

	    /**
	    @brief Возвращает массив указателей на слои бассейна расплава, заданные во входном файле.
	    */
	    std::vector <shared_ptr<MeltPoolLayer>> getMeltPoolLayers();
		bool checkMeltPoolLayerByName(string& mpl_name);

	    /**
	    @brief Возвращает массив указателей на бассейны расплава, заданные во входном файле.
	    */
	    std::vector <std::shared_ptr<MeltPool>> getMeltPools();
		
		/**
	    @brief Возвращает атмосферу, заданную во входном файле
	    */
		shared_ptr<Atmosphere_state> getAtmosphereState();
		std::vector <std::shared_ptr<Atmosphere_state>> getAtmosphereStates();
		shared_ptr<Atmosphere_state> getAtmosphereStateByName(string& atm_name);
		bool checkAtmosphereStateByName(string& atm_name);

	    /**
	    @brief Возвращает массив указателей на материалы, заданные во входном файле.
	    */
	    std::map <std::string, shared_ptr<StructuralMaterial>> getStructMaterials();

	    /**
		    @brief Возвращает массив указателей на продукты деления, заданные во входном файле.
		    */
	    std::map < std::string, shared_ptr<FissionProduct>> getFissionProducts();

	    /**
	    @brief Возвращает таблицу контроля шага, заданную во входном файле.
	    */
	    std::vector <shared_ptr<timeControlRow>> getTimeControlTable();

	    /**
	    @brief Возвращает активную зону. заданную во входном файле
	    */
	    shared_ptr<Core> getCore();
	    std::vector<std::vector<std::shared_ptr<RadiationExchangeViewFactor>>> getRadiationExchangeViewFactorsGroup();

	    int get_output_precision() { return output_precision; }

	    shared_ptr<DissociationModel> getDissociation();

	    shared_ptr<HeatModuleExtraOptions> getHeatModuleExtraOptions();

		shared_ptr<SecondaryCriticality> getSecondaryCriticality();

		shared_ptr<UN_O_Interaction::UN_O_InteractionModel> getUN_O_InteractionModel();
		vector<shared_ptr<UN_O_Interaction::UN_O_InteractionModel_HeatStruct_Link>> getUN_O_InteractionModel_HeatStruct_Links();
#ifdef _WIN32
		shared_ptr<Dissolution> getUO2_DissolutionModel();
#endif
	    // количество нитей omp  для распараллеливания по количеству HS
	    size_t get_OMPThreadsNumberHElement() { return OMPTreadsNumberHeatElement; };
	    // количество нитей omp для распараллеливания тепловой задачи
	    size_t get_OMPThreadsNumberHProblem() { return OMPTreadsNumberHeatProblem; };

	    bool is_SmartDBUsed() { return isSmartDB_used; };

	    NumScheme numerical_scheme;

	    std::vector<std::shared_ptr<HeatStruct_interTransfer>> getHeatStructInterTransfer();

	    std::vector<std::shared_ptr<HeatStructChangeFilmFlowability>> getHeatStructChangeFilmFlowability();
		/**
		@brief Конструктор класса
		@param inputFileName - имя входного файла,
		*/
		InputFileReader(const std::string& inputFileName) :
			  fileName(inputFileName)
			, inputCore(nullptr)
			, isActiveHydraulics(false)
			, OMPTreadsNumberHeatElement(1)
			, OMPTreadsNumberHeatProblem(1)
			, isSmartDB_used(false) {
			numerical_scheme = NumScheme::UPWIND;
		}

		virtual ~InputFileReader();
	
		vector<shared_ptr<timeControlRow>> timeControlTable; // таблица из входного файла для ограничения шага по времени
        int output_precision; //точность вывода данных (количество знаков после запятой)

		pugi::xml_node root;
		string fileName;
		string errorMessage;

		static const string material_data_base_file_name;
		static const string fission_product_data_base_file_name;

		map <string, shared_ptr<StructuralMaterial>> inputMaterials; /*!< материалы */
		map<string, shared_ptr<FissionProduct>> fissionProducts;     /*!< продукты деления */

		vector <shared_ptr<HeatElement>> inputHeatElements;			 /*!< тепловые структуры */
		vector <shared_ptr<HeatBound>> heatBoundsInInputFile;   /*!< граничные условия */
		vector <shared_ptr<FilmBound>> filmBounds;   /*!< граничные условия плёнки */
		vector <shared_ptr<GasBound>> gasFilmBounds; /*!< параметры газового потока */
		vector <shared_ptr<MeltPool>> meltPools;
		vector <shared_ptr<MeltPoolLayer>> meltPoolLayers;
		shared_ptr<Atmosphere_state> atmosphere_state;
		vector <shared_ptr<Atmosphere_state>> atmosphere_states;

		vector <shared_ptr<SAFRTable>> inputTables;
		vector <shared_ptr<MultiTable>> inputMultiTables;

		vector <shared_ptr<Connection>> connections;

		std::vector<std::vector<std::shared_ptr<RadiationExchangeViewFactor>>> radiationExchangeViewFactorsGroup;

		map<string, string> fuelRodList; // только при расчёте в связке SAFR-Беркут-DN3D, первый: полное имя твэла, второй: короткое
		map<string, string> CanisterList;
		shared_ptr<Core> inputCore; // только при расчёте в связке SAFR-Беркут-DN3D


		bool isActiveHydraulics; // подключен ли модуль Hydra, в зависимости от флага разный полный формат имен у твэлов!
								 // false: "Zone/_Assembly/_[rod name]"
								 // true: "[zone name]_[fuel assembly name]_[rod name]"

		bool isActiveNeutronics; // подключен ли модуль DN3D

		string atmosphereType; // тип атмосферы при диссоциации
		shared_ptr<DissociationModel> inputDiss;

	    shared_ptr<HeatModuleExtraOptions> extra_options;

		unordered_map<string, unordered_map<string, double>> inputNuclides; // concentration = inputNuclides["materialName"]["nuclideName"]

		shared_ptr<SecondaryCriticality> secondary_criticality; // данные для задачи вторичной критичности

	    std::vector<std::shared_ptr<HeatStruct_interTransfer>> input_hs_inter_transfer;

	    std::vector<std::shared_ptr<HeatStructChangeFilmFlowability>> input_hs_film_change_flowability;

	    std::shared_ptr<InputAxialManager> input_axial_manager;

	    std::shared_ptr<InputCoreCatcherStratification> input_core_catcher_stratification;

		std::shared_ptr<UN_O_Interaction::UN_O_InteractionModel> un_o_model;
		vector<std::shared_ptr<UN_O_Interaction::UN_O_InteractionModel_HeatStruct_Link>> un_o_h_struct_link;
#ifdef _WIN32
		std::shared_ptr<Dissolution> uo2_dissolution;
#endif
	    std::vector<std::shared_ptr<EutecticBase>> eutectic_vector; // набор всех заданных эвтектик

        /**
		@brief Cчитывание узлов <TimeTable>
		@details заполнение вектора timeControlTable
		*/	
        virtual bool readTimeTable(pugi::xml_node main, double start_time = 0.) = 0;
        virtual bool readCalculation(pugi::xml_node task) =0;
	    std::vector<std::shared_ptr<timeControlRow>> readControlTable(pugi::xml_node table, double start_time);

		/**
		@brief Cчитывание <Nuclides>
		@details заполнение inputNuclides map
		*/	
		bool readNuclides();

		/**
		@brief Cчитывание <HeatStruct>
		@details заполнение вектора inputHeatElements
		*/	
		bool readHeatStructs();

		/**
		@brief Считывание теловых структур в формате ввода модуля БЕРКУТ
		@details заполнение вектора inputHeatElements
		*/	
		bool readHeatStructsInBERKUTFormat(const BerkutParam &param, const map<string, string> &fuelRodList);

		/**
		@brief Считывание радиальной сетки теловой структуры в формате ввода модуля БЕРКУТ
		@details
		*/
		bool readRadLayCountInBERKUTFormat(const pugi::xml_node &mdl, size_t &Nr_hole, size_t &Nr_pellet, size_t &Nr_gap, size_t &Nr_cladding);

		/**
		@brief Считывание граничных условий всех подряд
		@details заполняет массив heatBounds
		*/	
		bool readHeatBounds();

		/**
		@brief Считывание граничных условий плёнки
		@details заполняет массив filmBounds
		*/	
		bool readFilmBounds();

		/**
		@brief Считывание бассейнов расплава
		@details заполняет массив meltPools
		*/
		void readMeltPools();
		void readMeltPoolLayers(pugi::xml_node &root);

		shared_ptr<TransportModule3D> readTransport3D(const pugi::xml_node &transport3D, const shared_ptr<MeltPoolLayerState> &state);
		vector<pair<Point, SimpleMatrixTemplate::Matrix<double>>> read_velocity_field(const pugi::xml_node &vector_field);

		vector<pair<Point, vector<double>>> read_fission_product_fields( const pugi::xml_node &scalar_field
																	   , const vector<shared_ptr<FissionProduct>> &product
																	   , const shared_ptr<MeltPoolLayerState> &state);

      	/**
		@brief Считывание газовой атмосферы над бассейном расплава
		@details заполняет AtmosphereState
		*/
		void readAtmosphereState(pugi::xml_node &root);
		void readAtmosphereStates(pugi::xml_node &root);

      	/**
		@brief Считывание граничных условий на границе плёнка-газ
		*/	
		bool readGasFilmBound();

		/**
		@brief Считывание связей
		*/	
		bool readConnections();

		/**
		@brief Считывание таблиц
		*/	
		bool readTables();

		/**
		@brief Считывание таблиц
		*/
		bool readMultiTables();

		/**
		@brief Считывание материалов
		*/	
		bool readMaterials(const string& module_name);

		/**
		@brief Считывание продуктов деления
		*/
		bool readFissionProducts(const string& module_name);

		/**
		@brief Считывание констант
		*/	
		bool readConstants();

		/**
		@brief Считывание дополнительных параметров расчёта
		*/
		void readNumericalParameters();

		/**
		@brief Cчитывание <Dissociation>
		@details
		*/
		void readDissociation();

		void read_UN_Pb_Steel_Ar_O2_interaction();

		/**
		@brief Cчитывание <UO2_Dissolution>
		@details
		*/
#ifdef _WIN32
	    void read_UO2_Dissolution();
#endif
		/**
		@brief Считывание параметров распараллеливания
		*/
		void readParallel();
	    /**
		@brief Считывание дополнительных опций теплового модуля 
		*/
	    void readHeatModuleExtraOptions();
		/**
		@brief Считывание параметров вторичной критичности
		*/
		void readSecondaryCriticality();

		/**
		@brief Считывание параметров для обмена между тепловыми структурами
		*/	
		void readHeatStructInterTransfer();

        /**
		@brief Считывание параметров для смены типа стекания 
		*/	
		void HeatStruct_ChangeFilmFlowability();


	    /**
		@brief Считывание параметров для аксиального переноса
		*/	
		void readAxialTransfer();
	    std::shared_ptr<InputAxialManager> getInputAxialManager();

		/**
		@brief Считывание параметров для стратификации материалов в ловушке расплава
		*/	
		void readCoreCatcherStratification();
	    std::shared_ptr<InputCoreCatcherStratification> getInputCoreCatcherStratification();

		/**
		@brief Считывание эвтектик
		*/	
		void readEutectics();
	    std::vector<std::shared_ptr<EutecticBase>> getInputEutectics();

		template<class Type, class XmlAttributeType>
		Type getFromCFuncOrReadTypeFromXmlNode(const string& attribute_name, const pugi::xml_node& node,
			XmlAttributeType (pugi::xml_attribute::* as_type)(XmlAttributeType) const, const XmlAttributeType &defaultVal) const;

		pugi::xml_node getRequiredChildInXmlNode(const string &child_name, const pugi::xml_node &node, const string &error_message = "") const;
		void checkRequiredAttributeName(const string &attribute_name, const pugi::xml_node &node, const string &error_message = "") const;
		int readRequiredIntAttributeFromXmlNode(const string &attribute_name, const pugi::xml_node &node, const string &error_message = "") const;
		string readRequiredStringAttributeFromXmlNode(const string &attribute_name, const pugi::xml_node &node, const string &error_message = "") const;
		double readRequiredDoubleAttributeFromXmlNode(const string &attribute_name, const pugi::xml_node &node, const string &error_message = "") const;

		template<class Type>
	    static void checkOptionalAttributeRange(const string &attribute_name, const pugi::xml_node &node, Type attribute_value, Type min_value, Type max_value, const string& error_message = "");

		static bool checkOptionalAttributeName(const string &attribute_name, const pugi::xml_node &node);
		string readOptionalStringAttributeFromXmlNode(const string &attribute_name, const pugi::xml_node &node, const string &default_value = "") const;
		double readOptionalDoubleAttributeFromXmlNode(const string &attribute_name, const pugi::xml_node &node, const double &default_value = 0.0) const;
		int readOptionalIntegerAttributeFromXmlNode(const string &attribute_name, const pugi::xml_node &node, const int &default_value = 1.0) const;

		static const string flag_ON;
		static const string flag_OFF;
		bool isRequiredFlagSwitchedOnInXmlNode(const string &flag_name, const pugi::xml_node &node, const string &on_value = flag_ON, const string &off_value = flag_OFF) const;
		bool isOptionalFlagSwitchedOnInXmlNode(const string &flag_name, const pugi::xml_node &node, const bool default_value = false, const string &on_value = flag_ON, const string &off_value = flag_OFF) const;
		VariableType VariableTypeForParameterInXmlNode(const string &parameter_name, const pugi::xml_node &node) const;

		void readFuelRodList();
	    bool isInFuelRodListByName(const string &rod_name);
		void readCanisterList();
		bool isInCanisterListByName(const string& name);
		void readAbsorbingRodList();
	    bool isInAbsRodListByName(const string &rod_name);
		bool readCore();
        bool readLinkChannelHS(pugi::xml_node &Links);
		void clearingCore();

		/**
		@brief Cчитывание атрибутов тэга <HeatStruct>
		@details Если считывание прошло успешно, возвращает 1.
		Иначе возваращет false, сообщение об ошибке может быть получено вызовом getErrorMessage()
		Данные сохранятются в полях соответствующего объекта HeatStruct.
		*/	
		bool readHeatStructAttributes(pugi::xml_node heatStruct, const shared_ptr<HeatStruct> &heatElement);

		/**
		@brief Cчитывание атрибутов тэга <FuelRod> в формате ввода модуля БЕРКУТ
		@details Если считывание прошло успешно, возвращает 1.
		Иначе возваращет false, сообщение об ошибке может быть получено вызовом getErrorMessage()
		Данные сохранятются в полях соответствующего объекта HeatStruct.
		*/	
		bool readRodMeshAndMaterial(const pugi::xml_node &rod, const shared_ptr<HeatStruct> &heatElement, const BerkutParam &par);
	
		/**
		@brief Cчитывание атрибутов тэгов <Mesh> в формате ввода модуля БЕРКУТ
		@details Возвращает длину уже обработанной части тепловой структуры
		Данные сохранятются в полях соответствующего объекта HeatStruct.
		*/
		double readAxialRodPart(const pugi::xml_node &rodPart, const shared_ptr<HeatStruct> &heatElement, const BerkutParam &par, const double &z);

		/**
		@brief Проверка параметров наличия материалов и корректности их параметров, введенных через модуль БЕРКУТ
		@details Если материала не было, добавляем его в набор inputMaterials
		Возвращает false при ошибке
		*/
		void check_materials(const std::vector<std::shared_ptr<MaterialsParam>> &berkut_materials);

		/**
		@brief Cчитывание атрибутов тэга <AxialSection>
		@details Если считывание прошло успешно, возвращает 1.
		Иначе возваращет false, сообщение об ошибке может быть получено вызовом getErrorMessage()
		Данные сохранятются в полях соответствующего объекта HeatStruct.
		*/	
		bool readAxialSectionAttributes(pugi::xml_node axialSection, const shared_ptr<HeatStruct> &heatElement);

		/**
		@brief Cчитывание атрибутов тэга <RadialSection>
		@details Если считывание прошло успешно, возвращает 1.
		Иначе возваращет false, сообщение об ошибке может быть получено вызовом getErrorMessage()
		Данные сохранятются в полях соответствующего объекта HeatStruct.
		*/	
		bool readRadialSectionAttributes(pugi::xml_node radialSection, const shared_ptr<HeatStruct> &heatElement);

		/**
		@brief Cчитывание атрибутов тэга <Region>
		@details Если считывание прошло успешно, возвращает 1.
		Иначе возваращет false, сообщение об ошибке может быть получено вызовом getErrorMessage()
		Данные сохранятются в полях соответствующего объекта Region. Указатели на region одного
		теплового элемента хранятся в векторе HeatStruct.regions
		*/	
		bool readRegionAttributes(pugi::xml_node region, const shared_ptr<HeatStruct> &heatElement);

		// Абудалипов Эмиль, измнения таблетки в heatstruct 
		/**
		@brief Cчитывание атрибутов тэга <Pellet>
		@details Если считывание прошло успешно, возвращает 1.
		Иначе возваращет false, сообщение об ошибке может быть получено вызовом getErrorMessage()
		Данные сохранятются в полях соответствующего объекта Pellet. Указатели на region одного
		теплового элемента хранятся в векторе HeatStruct.regions
		*/
		bool readPelletAttribute(pugi::xml_node pellet, const shared_ptr<HeatStruct> &heatElement);

		double where_to_put_pellet = 0;
		//-----------------------------------------------------------------------

		/**
		@brief Функция для чтения аттрибутов свойств материала MatProp
		@details Возвращает пару значений (температура, значение теплофизического свойства) 
		*/
		pair<double, double> readMaterialPropertyLine(pugi::xml_node matProp);

		/**
		@brief Функции для чтения аттрибутов свойств материала MatProp
		@details Считывают указанное свойство материала и проверяет, что температура в таблице задана по возрастанию
		и не повторяется (кроме точки плавления)
		*/
		vector <pair<double, double>> readRequiredMaterialProperty(pugi::xml_node material, const pugi::char_t *prop, const double &temp_melt, const string &material_name);
		vector <pair<double, double>> readOptionalMaterialProperty(pugi::xml_node material, const pugi::char_t *prop, const double &temp_melt, const string &material_name);

		/**
		@brief Функция для чтения заданных материалов MatProp
		@details Возвращает пару значений (температура, значение теплофизического свойства) 
		*/	
		bool readMaterial(pugi::xml_node material, shared_ptr<InputMaterial> inputMaterial);

        MeltFlowability readMeltFlowability(pugi::xml_node material);

		bool readTable(pugi::xml_node table, shared_ptr<SAFRTable> new_table);
		bool readMultiTables(pugi::xml_node &table, shared_ptr<MultiTable> &new_table);
		bool readTableInBERKUTFormat(pugi::xml_node &table, SAFRTable &new_table);
		vector<double> readStringOfNumbersInBERKUTTable(pugi::xml_node &string_node);

		bool createConnections();

		vector<double> ConvertStringCSVToDoubleArray(string str); // str - строка с числами, разделенными запятыми
		vector<double> ConvertStringSCVToArray(string &str); // str - строка с числами, разделенными пробелами
		vector<size_t> ConvertStringCSVToIntArray(string str); // str - строка с числами, разделенными запятыми

        std::shared_ptr<Canister> readCoreCanister(pugi::xml_node& core);
		
		size_t OMPTreadsNumberHeatElement; // количество нитей omp  для распараллеливания по количеству HS
		size_t OMPTreadsNumberHeatProblem; // количество нитей omp для распараллеливания тепловой задачи
		bool isSmartDB_used; // флаг для определения используемли мы SmartDB или нет. Необходимо для распараллеливания.

	    template<typename T>
		static void check_range(const T &value, const T &value_min, const T &value_max, const string &info, const string &value_name) {
			if (value < value_min || value > value_max) {
				throw std::invalid_argument(info + ": " + value_name + " is out of range");
			}
	    }

		template<typename T>
		void readPropertyFromXmlNode( pugi::xml_node &node
								    , T *object
									, const pugi::char_t *property_name
									, void  (T::*set_property_method)(const double &value)
									, void  (T::*set_flag_method)(const VariableType value)
									, void  (T::*set_table_method)(const shared_ptr<InputTableInterface> &value)
			                        , void  (T::*set_cf_name)(const std::string& inpunt_name)
									, const string &error_message = "") {

		    VariableType type = VariableTypeForParameterInXmlNode(property_name, node);
			(object->*set_flag_method)(type);

			double property_initial = UNDEF_DOUBLE;
			if (type == VariableType::Table) {
				string tableName = readRequiredStringAttributeFromXmlNode(property_name, node, error_message);
                std::shared_ptr<SAFRTable> property_table = find_object_by_name<SAFRTable>(tableName, inputTables);

                if (property_table != nullptr) {
                    (object->*set_table_method)(property_table);
                    property_initial = property_table->table[0].second;
                }
                else {
                    size_t index = cfunc.table_index(tableName);
					std::shared_ptr<ControlFunctionTable> table = std::make_shared<ControlFunctionTable>(tableName);
					(object->*set_table_method)(table);
                }

			}
            else if (type == VariableType::ControlFunction) {
                string cf_name = readRequiredStringAttributeFromXmlNode(property_name, node, error_message);
                (object->*set_cf_name)(cf_name);
            }
			else if(type == VariableType::Const) {
				property_initial = readRequiredDoubleAttributeFromXmlNode(property_name, node, error_message);
			}

			(object->*set_property_method)(property_initial);
		}
};

template<class Type, class XmlAttributeType>
Type InputFileReader::getFromCFuncOrReadTypeFromXmlNode(const string& attribute_name, const pugi::xml_node& node,
	XmlAttributeType (pugi::xml_attribute::*as_type)(XmlAttributeType) const, const XmlAttributeType& defaultVal) const {

	auto attribute = node.attribute(attribute_name.c_str());
	double value = UNDEF_DOUBLE;
	cfunc.get_value(attribute.as_string(), value);

	if (!IS_UNDEF_DOUBLE(value)) {
		return static_cast<Type>(value);
	}
	else {
		return static_cast<Type>((attribute.*as_type)(defaultVal));
	}
}

// Task Version 3.2
class InputFileReader32 : public InputFileReader {
public:
    InputFileReader32(const std::string& inputFileName) : InputFileReader(inputFileName) {}
    ~InputFileReader32() = default;

    bool readCalculation(pugi::xml_node main) override;
	bool readTimeTable(pugi::xml_node main, double start_time = 0.0) override;
};

// Task Version 3.8
class InputFileReader38 : public InputFileReader {
public:
    InputFileReader38(const std::string& inputFileName) : InputFileReader(inputFileName) {}
    ~InputFileReader38() = default;

    bool readCalculation(pugi::xml_node main) override;
	bool readTimeTable(pugi::xml_node main, double start_time) override;
};

template<class Type>
void InputFileReader::checkOptionalAttributeRange(const string &attribute_name,
	                                              const pugi::xml_node &node,
	                                              Type attribute_value,
	                                              Type min_value,
	                                              Type max_value,
	                                              const string &error_message) {

	if (!checkOptionalAttributeName(attribute_name, node)) {
		return;
	}

	if (attribute_value < min_value || attribute_value > max_value) {
		string message = error_message;
		if (message.empty()) {
			message = "ERROR: " + attribute_name + ": out of range";
		}

		throw std::invalid_argument(error_message);
	}
}