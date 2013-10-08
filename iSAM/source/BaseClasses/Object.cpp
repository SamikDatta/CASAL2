/**
 * @file BaseObject.cpp
 * @author  Scott Rasmussen (scott.rasmussen@zaita.com)
 * @version 1.0
 * @date 18/09/2012
 * @section LICENSE
 *
 * Copyright NIWA Science �2012 - www.niwa.co.nz
 *
 * $Date: 2008-03-04 16:33:32 +1300 (Tue, 04 Mar 2008) $
 */

// Headers
#include "Object.h"

#include "Utilities/Logging/Logging.h"
#include "Utilities/To.h"

namespace isam {
namespace base {
/**
 * Default Constructor
 */
Object::Object() {
}

/**
 * Destructor
 */
Object::~Object() {
}


/**
 * Print the description and usage details for this object.
 */
void Object::PrintDescription() const {
  cout << "Object Description: \n";
  cout << "Description: " << description_ << endl;

  // TODO: Add parameter printing
}

/**
 * This method will check if we have a parameter defined
 * and if not log an error
 *
 * @param label The label of the parameter to check
 */
void Object::CheckForRequiredParameter(const string &label) const {
  if (!parameters_.IsDefined(label))
    LOG_ERROR("At line " << defined_line_number_ << " in file " << defined_file_name_
        <<
        ": " << block_type_ << " block requires the '" << label << "' parameter");
}

/**
 * This method will check to see if the estimable label passed
 * in is registered as part of a vector or not.
 *
 * @param label The label of the estimable to check
 */
bool Object::IsEstimableAVector(const string& label) const {
  map<string, unsigned>::const_iterator iter = estimable_sizes_.find(label);
  if (iter == estimable_sizes_.end()) {
    LOG_CODE_ERROR("Unable to locate the label " << label << " in our estimable_sizes map");
  }

  return (iter->second > 1);
}

/**
 *
 */
unsigned Object::GetEstimableSize(const string& label) const {
  map<string, unsigned>::const_iterator iter = estimable_sizes_.find(label);
  if (iter == estimable_sizes_.end()) {
    LOG_CODE_ERROR("Unable to locate the label " << label << " in our estimable_sizes map");
  }

  return iter->second;
}

/**
 * This method will find the estimable with the matching
 * label and return it.
 *
 * @param label The label of the estimable to find
 * @return A pointer to the estimable to be used by the Estimate object
 */
Double* Object::GetEstimable(const string& label) {
  if (estimables_.find(label) == estimables_.end()) {
    LOG_CODE_ERROR("Unable to find the estimable with the label: " << label);
  }

  return estimables_[label];
}

/**
 * This method will register a variable as an object
 * that can be targeted by an estimate to be used as part of an
 * estimation process or MCMC.
 *
 * @param label The label to register the estimable under
 * @param variable The variable to register as an estimable
 */
void Object::RegisterAsEstimable(const string& label, Double* variable) {
  estimable_sizes_[label] = 1;
  estimables_[label]      = variable;
}

/**
 * This method will register a vector of variables as an object
 * that can be targeted by an estimate to be used as part of an
 * estimation process or MCMC.
 *
 * @param label The label to register the estimable under
 * @param variables Vector containing all the elements to register
 */
void Object::RegisterAsEstimable(const string& label, vector<Double>& variables) {
  estimable_sizes_[label] = variables.size();

  for(unsigned i = 0; i < variables.size(); ++i) {
    string new_label = label + "(" + utilities::ToInline<unsigned, string>(i + 1) + ")";
    RegisterAsEstimable(new_label, &variables[i]);
  }
}

/**
 * This method will register a map of variables as estimables.
 * When register each variable it'll be done like:
 *
 * process_label.variable(map.string)
 *
 * @param label The label for the process
 * @param variables Map containing index and double values to store
 */
void Object::RegisterAsEstimable(const string& label, map<string, Double>& variables) {
  estimable_sizes_[label] = variables.size();

  for (auto iterator = variables.begin(); iterator != variables.end(); ++iterator) {
    string new_label = label + "(" + iterator->first + ")";
    RegisterAsEstimable(new_label, &iterator->second);
  }
}

/**
 * This method will print the same value as the locations() method on the ParameterList for a given
 * parameter except it'll doing it for the whole base object
 */
string Object::location() {
  string line_number;
  isam::utilities::To<unsigned, string>(defined_line_number_, line_number);

  string location = "At line " + line_number + " in file " + defined_file_name_;
  if (label_ != "")
    location += ": " + label_;

  return location;
}

} /* namespace base */
} /* namespace isam */
