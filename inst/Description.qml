import QtQuick
import JASP.Module

Description
{
	name		: "jaspBayesianQualityControl"
	title		: qsTr("Bayesian Quality Control")
	description	: qsTr("Bayesian analyses for investigating whether a manufactured product adheres to a defined set of quality criteria")
	version		: "0.1.0"
	author		: "JASP Team"
	maintainer	: "JASP <info@jasp-stats.org>"
	website		: "https://github.com/jasp-stats/jaspBayesianQualityControl"
	license		: "GPL (>= 2)"
	icon		: "qualityControl-module.svg"
	hasWrappers	: false
	preloadData	: false

	GroupTitle
	{
		title:			qsTr("Capability Analysis")
		icon:			"qualityControl-capability.svg"
	}

	Analysis
	{
		title:			qsTr("Bayesian Process Capability Study")
		qml:			"BayesianProcessCapabilityStudies.qml"
		func:			"bayesianProcessCapabilityStudies"
		preloadData:	true
	}
}
