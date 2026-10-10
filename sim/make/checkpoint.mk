# Opt-in Level-1 VCS support. Separate binary/csrc names preserve ordinary builds.
ifneq ($(strip $(FS_CHECKPOINT_PROFILE)),)
ifneq ($(FS_CHECKPOINT_PROFILE),fcfs-fased-v1)
$(error Unsupported FS_CHECKPOINT_PROFILE: $(FS_CHECKPOINT_PROFILE))
endif
ifneq ($(TARGET_PROJECT):$(DESIGN):$(TARGET_CONFIG):$(PLATFORM_CONFIG),fasedtests:AXI4Fuzzer:FCFSConfig:FCFS16GBQuadRank_DefaultF1Config)
$(error FS_CHECKPOINT_PROFILE=fcfs-fased-v1 requires the audited AXI4Fuzzer FCFS/FASED configuration)
endif
ifneq ($(filter verilator% xcelium% xsim% vcs-debug vcs-post-synth% driver $(PLATFORM),$(MAKECMDGOALS)),)
$(error FS_CHECKPOINT_PROFILE supports non-debug RTL VCS metasim only)
endif
ifneq ($(strip $(SIMULATOR_CHECKPOINT_INVENTORY)),)
ifneq ($(SIMULATOR_CHECKPOINT_INVENTORY),FPGATop)
$(error The Level-1 checkpoint profile requires the complete FPGATop inventory)
endif
endif
SIMULATOR_CHECKPOINT_INVENTORY := FPGATop
CHECKPOINT_TOOLS_DIR := $(abspath $(firesim_base_dir)/../../../tools/checkpoint_formats)
endif
