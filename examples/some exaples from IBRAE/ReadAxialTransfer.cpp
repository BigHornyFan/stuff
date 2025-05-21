#include "InputFileReader.h"

void InputFileReader::readAxialTransfer() {
    // считывание парметров для аксильного переноса

    pugi::xml_node axial_transfer = root.child("AxialTransfer");

    if (axial_transfer.empty()) {
        return;
    }

    std::vector<std::shared_ptr<TransferBlock>> input_blocks;
    for (pugi::xml_node transfer_block = axial_transfer.child("TransferBlock"); transfer_block; transfer_block = transfer_block.next_sibling("TransferBlock")) {
        string obj_type = readRequiredStringAttributeFromXmlNode("Type", transfer_block, "ERROR: No Type in TransferBlock for AxialTransfer.");
        string full_name_from;
        if (obj_type == "FuelRod") {
            string zone_name = readRequiredStringAttributeFromXmlNode("Zone", transfer_block, "ERROR: No Zone in TransferBlock for AxialTransfer.");
            string assambly_name = readRequiredStringAttributeFromXmlNode("FuelAssembly", transfer_block, "ERROR: No FuelAssembly in TransferBlock for AxialTransfer.");
            string name_from = readRequiredStringAttributeFromXmlNode("Name", transfer_block, "ERROR: No Name in TransferBlock for AxialTransfer.");
            full_name_from = zone_name + "_" + assambly_name + "_" + name_from;
        }
        else if (obj_type == "Canister") {
            string zone_name = readRequiredStringAttributeFromXmlNode("Zone", transfer_block, "ERROR: No Zone in TransferBlock for AxialTransfer.");
            string name_from = readRequiredStringAttributeFromXmlNode("Name", transfer_block, "ERROR: No Name in TransferBlock for AxialTransfer.");
            full_name_from = name_from + "_" + zone_name;
        }
        else if (obj_type == "HeatStruct") {
            string name_from = readRequiredStringAttributeFromXmlNode("Name", transfer_block, "ERROR: No Name in TransferBlock for AxialTransfer.");
            full_name_from = name_from;
        }
        else {
            throw std::invalid_argument("ERROR: Incorrect TypeFrom in TransferParam for AxialTransfer.");
        }

        bool dynamic_mesh = isOptionalFlagSwitchedOnInXmlNode("DynamicMesh", transfer_block, false, "ON", "OFF"); 

        if (!dynamic_mesh) {
            double start_time = transfer_block.attribute("StartTransferTime").as_double();
            double z_stop = transfer_block.attribute("StopCoordinate").as_double();

            double r_in = transfer_block.attribute("Rin").as_double();
            double r_ex = transfer_block.attribute("Rex").as_double();
            double z_up = transfer_block.attribute("Zup").as_double();
            double z_down = transfer_block.attribute("Zdown").as_double();

            auto block = std::make_shared<TransferBlock>(full_name_from, start_time, z_stop, r_in, r_ex, z_up, z_down);
            input_blocks.push_back(block);
        }
    }

    if (input_blocks.empty()) {
        input_axial_manager = std::make_shared<InputAxialManager>();
    }
    else {
        input_axial_manager = std::make_shared<InputAxialManager>(input_blocks);
    }
}

std::shared_ptr<InputAxialManager> InputFileReader::getInputAxialManager() {
    return input_axial_manager;
}