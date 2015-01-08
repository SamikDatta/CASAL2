/**
 * @file TransitionCategoryByAge.h
 * @author Scott Rasmussen (scott.rasmussen@zaita.com)
 * @github https://github.com/Zaita
 * @date 10/12/2014
 * @section LICENSE
 *
 * Copyright NIWA Science �2014 - www.niwa.co.nz
 *
 * @section DESCRIPTION
 *
 * << Add Description >>
 */
#ifndef PROCESSES_TRANSITIONCATEGORYBYAGE_H_
#define PROCESSES_TRANSITIONCATEGORYBYAGE_H_

// headers
#include "Partition/Accessors/Categories.h"
#include "Penalties/Penalty.h"
#include "Processes/Process.h"
#include "Selectivities/Selectivity.h"

// namespaces
namespace niwa {
namespace processes {

namespace accessor = niwa::partition::accessors;

/**
 * Class definition
 */
class TransitionCategoryByAge : public niwa::Process {
public:
  // methods
  TransitionCategoryByAge();
  virtual                     ~TransitionCategoryByAge() = default;
  void                        DoValidate() override final;
  void                        DoBuild() override final;
  void                        DoReset() override final { };
  void                        DoExecute() override final;

private:
  // members
  vector<string>                from_category_labels_;
  vector<string>                to_category_labels_;
  vector<unsigned>              years_;
  unsigned                      min_age_;
  unsigned                      max_age_;
  string                        selectivity_label_;
  SelectivityPtr                selectivity_;
  string                        penalty_label_;
  PenaltyPtr                    penalty_;
  Double                        u_max_;
  parameters::TablePtr          n_table_;
  map<unsigned, vector<Double>> n_;
  accessor::Categories          to_partition_;
  accessor::Categories          from_partition_;
  ModelPtr                      model_;
};

} /* namespace processes */
} /* namespace isam */

#endif /* PROCESSES_TRANSITIONCATEGORYBYAGE_H_ */
