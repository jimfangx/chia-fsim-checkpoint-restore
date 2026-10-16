# See LICENSE for license details.

####################################
# Golden Gate Invocation           #
####################################

# Simulation memory map emitted by the MIDAS compiler
header := $(GENERATED_DIR)/$(BASE_FILE_NAME).const.h

# The midas-generated simulator RTL which will be baked into the FPGA shell project
simulator_verilog := $(GENERATED_DIR)/$(BASE_FILE_NAME).sv
simulator_xdc := $(GENERATED_DIR)/$(BASE_FILE_NAME).synthesis.xdc

# Request the inventory outputs as well, so enabling discovery also rebuilds
# an existing simulator generated without discovery.
ifneq ($(strip $(SEMANTIC_CHECKPOINT_ROOT)),)
ifneq ($(strip $(SEMANTIC_BRIDGE_CHECKPOINT_ROOT)),)
$(error Semantic checkpoint inventory requires exactly one root phase)
endif
endif
ifneq ($(strip $(SEMANTIC_MULTICLOCK_INVENTORY)),)
ifneq ($(SEMANTIC_MULTICLOCK_INVENTORY),1)
$(error SEMANTIC_MULTICLOCK_INVENTORY must be 1 or empty)
endif
ifeq ($(strip $(SEMANTIC_CHECKPOINT_ROOT) $(SEMANTIC_BRIDGE_CHECKPOINT_ROOT)),)
$(error SEMANTIC_MULTICLOCK_INVENTORY requires an explicit semantic root)
endif
endif
semantic_outputs := $(if $(strip $(SEMANTIC_CHECKPOINT_ROOT) $(SEMANTIC_BRIDGE_CHECKPOINT_ROOT)),$(addprefix $(GENERATED_DIR)/$(BASE_FILE_NAME),.semantic-state.json .semantic-reference.fir .semantic-clean.sv .semantic-boundary.json),)
semantic_outputs += $(if $(SEMANTIC_MULTICLOCK_INVENTORY),$(GENERATED_DIR)/$(BASE_FILE_NAME).semantic-clean-access.json,)
ifneq ($(strip $(SEMANTIC_FAME_STATE_MAP)),)
ifneq ($(SEMANTIC_FAME_STATE_MAP),1)
$(error SEMANTIC_FAME_STATE_MAP must be 1 or empty)
endif
ifeq ($(strip $(SEMANTIC_BRIDGE_CHECKPOINT_ROOT)),)
$(error SEMANTIC_FAME_STATE_MAP requires an explicit bridge-extracted semantic root)
endif
ifneq ($(SEMANTIC_MULTICLOCK_INVENTORY),1)
$(error SEMANTIC_FAME_STATE_MAP requires SEMANTIC_MULTICLOCK_INVENTORY=1)
endif
semantic_outputs += $(GENERATED_DIR)/$(BASE_FILE_NAME).semantic-fame-map.json
semantic_outputs += $(GENERATED_DIR)/$(BASE_FILE_NAME).semantic-fame-emitted.sv
endif
simulator_inventory_outputs := $(if $(SIMULATOR_CHECKPOINT_INVENTORY),$(addprefix $(GENERATED_DIR)/$(BASE_FILE_NAME),.simulator-state.json .simulator-reference.fir),)

# Pre-simulation-mapping annotations which includes all Bridge Annotations
# extracted used to generate new runtime configurations.
fame_annos := $(GENERATED_DIR)/post-bridge-extraction.json

.PHONY: verilog compile
verilog: $(simulator_verilog) $(semantic_outputs) $(simulator_inventory_outputs)
compile: $(simulator_verilog) $(semantic_outputs) $(simulator_inventory_outputs)

# Disable FIRRTL 1.4 deduplication because it creates multiple failures
# Run the 1.3 version instead (checked-in). If dedup must be completely disabled,
# pass --no-legacy-dedup as well
$(simulator_verilog) $(simulator_xdc) $(header) $(fame_annos) $(semantic_outputs) $(simulator_inventory_outputs) &: $(FIRRTL_FILE) $(ANNO_FILE) $(FIRESIM_MAIN_CP)
	$(call run_jar_scala_main,$(firesim_base_dir),$(FIRESIM_MAIN_CP),midas.stage.GoldenGateMain,\
		-i $(FIRRTL_FILE) \
		-td $(GENERATED_DIR) \
		-faf $(ANNO_FILE) \
		-ggcp $(PLATFORM_CONFIG_PACKAGE) \
		-ggcs $(PLATFORM_CONFIG) \
		--output-filename-base $(BASE_FILE_NAME) \
		--allow-unrecognized-annotations \
		$(if $(SEMANTIC_CHECKPOINT_ROOT),--semantic-checkpoint-root $(SEMANTIC_CHECKPOINT_ROOT),) \
		$(if $(SEMANTIC_BRIDGE_CHECKPOINT_ROOT),--semantic-bridge-checkpoint-root $(SEMANTIC_BRIDGE_CHECKPOINT_ROOT),) \
		$(if $(SEMANTIC_MULTICLOCK_INVENTORY),--semantic-multiclock-inventory,) \
		$(if $(SEMANTIC_FAME_STATE_MAP),--semantic-fame-state-map,) \
		$(if $(SIMULATOR_CHECKPOINT_INVENTORY),--simulator-checkpoint-inventory $(SIMULATOR_CHECKPOINT_INVENTORY),) \
		--no-dedup)
	grep -sh ^ $(GENERATED_DIR)/firrtl_black_box_resource_files.f | \
		xargs cat >> $(simulator_verilog) # Append blackboxes to FPGA wrapper, if any

####################################
# Runtime-Configuration Generation #
####################################

# This reads in the annotations from a generated target, elaborates a
# FASEDTimingModel if a BridgeAnnoation for one exists, and asks for user input
# to generate a runtime configuration that is compatible with the generated
# hardware (BridgeModule). Useful for modelling a memory system that differs from the default.
.PHONY: conf
conf: $(fame_annos) $(FIRESIM_MAIN_CP)
	mkdir -p $(GENERATED_DIR)
	$(call run_jar_scala_main,$(firesim_base_dir),$(FIRESIM_MAIN_CP),midas.stage.RuntimeConfigGeneratorMain,\
		-td $(GENERATED_DIR) \
		-faf $(fame_annos) \
		-ggcp $(PLATFORM_CONFIG_PACKAGE) \
		-ggcs $(PLATFORM_CONFIG))
