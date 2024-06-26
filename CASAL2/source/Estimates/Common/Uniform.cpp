/**
 * @file Uniform.cpp
 * @author  Scott Rasmussen (scott.rasmussen@zaita.com)
 * @version 1.0
 * @date 8/03/2013
 * @section LICENSE
 *
 * Copyright NIWA Science �2013 - www.niwa.co.nz
 *
 * $Date: 2008-03-04 16:33:32 +1300 (Tue, 04 Mar 2008) $
 */

// Headers
#include "Uniform.h"

// Namespaces
namespace niwa {
namespace estimates {

/**
 * Default constructor
 */
Uniform::Uniform(shared_ptr<Model> model) : Estimate(model) {}


/**
 * GetPriorValues()
 *
 * @return empty vector no prior parameters for this estimate type
 */
vector<Double>   Uniform::GetPriorValues() {
  vector<Double> result = {0};
  return result;
}
vector<string>   Uniform::GetPriorLabels() {
  vector<string> result = {"none"};
  return result;
}


} /* namespace estimates */
} /* namespace niwa */
