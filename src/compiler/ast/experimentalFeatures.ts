import { createMapFromTemplate } from "../core";
import { Map } from "../types";

export const enum ExperimentalFeature {
    SMTChecker,
    ABIEncoderV2, // new ABI encoder that makes use of JULIA
    V050, // v0.5.0 breaking changes
    Test,
    TestOnlyAnalysis
}

export const experimentalFeatureNames: Map<ExperimentalFeature> = createMapFromTemplate({
    "SMTChecker": ExperimentalFeature.SMTChecker,
    "ABIEncoderV2": ExperimentalFeature.ABIEncoderV2,
    "v0.5.0": ExperimentalFeature.V050,
    "__test": ExperimentalFeature.Test,
    "__testOnlyAnalysis": ExperimentalFeature.TestOnlyAnalysis
});
