/**
 * @file MortalityInstantaneousRetained.Test.cpp
 * @author Samik Datta (samik.datta@niwa.co.nz)
 * @github https://github.com/SamikDatta
 * @date 07/05/2019
 * @section LICENSE
 *
 * Copyright NIWA Science �2019 - www.niwa.co.nz
 *
 */
#ifdef TESTMODE

// Headers
#include "MortalityInstantaneousRetained.h"

#include "ObjectiveFunction/ObjectiveFunction.h"
#include "Processes/Manager.h"
#include "Model/Model.h"
#include "Partition/Partition.h"
#include "TestResources/TestFixtures/InternalEmptyModel.h"


// Namespaces
namespace niwa {
namespace processes {
namespace age {

using niwa::testfixtures::InternalEmptyModel;

const std::string test_cases_process_mortality_instantaneous_retained =
R"(
@model
start_year 2001 
final_year 2005
min_age 1
max_age 20 
age_plus true
base_weight_units tonnes
initialisation_phases Define_manually
time_steps 1

@categories 
format sex
names   male
age_lengths asMm0 

@initialisation_phase Define_manually
type state_category_by_age
categories male
min_age 1
max_age 10
table n
male 100000 90000 80000 70000 60000 50000 40000 30000 20000 10000
end_table

@time_step 1 
processes mort

@process mort
type mortality_instantaneous_retained
m 0
time_step_ratio 1
selectivities One
categories *
table catches
year FishingPot
2005  8015.123
end_table
table method
method    category    selectivity     retained_selectivity      u_max time_step     penalty
FishingPot    format=male      potFSel_length_male  potRet_male 0.7   1   CatchMustBeTaken1
end_table

@selectivity One
type constant
c 1

@selectivity potFSel_length_male
type logistic 
a50   4
ato95  1
length_based False

@selectivity potRet_male
type logistic 
a50   6
ato95  1
length_based False

@penalty CatchMustBeTaken1
type process
log_scale True
multiplier 1000

@age_length asMm0
type von_bertalanffy
by_length true
linf 55.7
k 0.14
t0 -0.82
cv_first 0.1
cv_last 0.1
length_weight Length_Weight

@length_weight Length_Weight
type basic
units kgs
a 0.007289
b 3.2055

@report population_sizes
type partition
time_step 1
years 2001:2005
)";


TEST_F(InternalEmptyModel, Processes_Mortality_Instantaneous_Retained_Simple) {
  AddConfigurationLine(test_cases_process_mortality_instantaneous_retained, __FILE__, 31);
  LoadConfiguration();

  model_->Start(RunMode::kBasic);

  vector<Double> expected = { 99998.976415, 89982.542510, 79719.128377, 67542.373295, 55997.579365, 46498.803312, 37191.693199, 27893.478988,
      18595.642450, 9297.820941, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}; // R code created by Ian, numbers match within 0.01% so using those numbers

  partition::Category& stock = model_->partition().category("male");
  for (unsigned i = 0; i < expected.size(); ++i) {
//    EXPECT_DOUBLE_EQ(expected[i], stock.data_[i]) << " with i = " << i;
    EXPECT_NEAR(expected[i], stock.data_[i], 1e-6) << " with i = " << i;
  }
}

} /* namespace age */
} /* namespace processes */
} /* namespace niwa */
#endif /* TESTMODE */
