// headers
#include "../../Model/Model.h"

// namespaces
namespace niwa::processes::verification {

// Function List
void AgeingProcessExists(shared_ptr<Model> model);
void RecruitmentCategoriesVerification(shared_ptr<Model> model);
void MortalityChecks(shared_ptr<Model> model);

// Execute the functions
void DoVerification(shared_ptr<Model> model) {
  AgeingProcessExists(model);
  RecruitmentCategoriesVerification(model);
  MortalityChecks(model);
}

}  // namespace niwa::processes::verification