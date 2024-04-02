/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <utility>
#include <filesystem>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <gtest/gtest.h>
#include "knowrob/integration/python/utils.h"

#include "knowrob/ontologies/DataSource.h"
#include "knowrob/semweb/OntologyLanguage.h"
#include "knowrob/ontologies/OntologyFile.h"
#include "knowrob/ontologies/SPARQLService.h"
#include "knowrob/triples/GraphSelector.h"

#define DATA_SOURCE_SETTING_FORMAT "format"
#define DATA_SOURCE_SETTING_LANG "language"
#define DATA_SOURCE_SETTING_TYPE "type"
#define DATA_SOURCE_SETTING_FRAME "frame"

#define DATA_SOURCE_TYPE_SPARQL "sparql"
#define DATA_SOURCE_TYPE_ONTOLOGY "ontology"

using namespace knowrob;
namespace fs = std::filesystem;
namespace qi = boost::spirit::qi;
namespace ascii = boost::spirit::ascii;

DataSource::DataSource(URI uri, std::string_view format, DataSourceType dataSourceType)
		: dataSourceType_(dataSourceType),
		  uri_(std::move(uri)),
		  format_(format) {
}

std::string DataSource::getNameFromURI(const std::string &uriString) {
	return fs::path(uriString).stem();
}

template<typename TP>
std::time_t to_time_t(TP tp) {
	// needed to convert file modification time to string
	using namespace std::chrono;
	auto sctp = time_point_cast<system_clock::duration>(tp - TP::clock::now() + system_clock::now());
	return system_clock::to_time_t(sctp);
}

std::string DataSource::getVersionFromURI(const std::string &uriString) {
	fs::path p(uriString);

	// check if it is a local existing file and use file modification time
	// as version in this case.
	if (exists(p)) {
		std::ostringstream oss;
		auto stamp = last_write_time(p);
		auto tt = to_time_t(stamp);
		auto tm = *std::localtime(&tt);
		oss << std::put_time(&tm, "%c");
		return oss.str();
	}

	// try to extract version from URI
	auto versionString = p.parent_path().filename();
	if (isVersionString(versionString)) {
		return versionString;
	}

	// fallback to use the current day as version, thus causing
	// a reload each day.
	{
		std::ostringstream oss;
		auto t = std::time(nullptr);
		auto tm = *std::localtime(&t);
		oss << std::put_time(&tm, "%d-%m-%Y");
		return oss.str();
	}
}

bool DataSource::isVersionString(const std::string &versionString) {
	// parser rules
	qi::rule<std::string::const_iterator> numbers = (
			(+qi::digit >> '.' >> +qi::digit >> '.' >> +qi::digit) |
			(+qi::digit >> '.' >> +qi::digit));
	// parse input
	auto first = versionString.begin();
	auto last = versionString.end();
	bool r = qi::phrase_parse(first,
							  last,
							  (('v' >> numbers) | numbers),
							  ascii::space);
	// return true if parser succeeded
	return (first == last && r);
}

static bool isOntologySourceType(
		const std::string &format,
		const boost::optional<std::string> &language,
		const boost::optional<std::string> &type) {
	if (type && (type.value() == DATA_SOURCE_TYPE_ONTOLOGY || type.value() == DATA_SOURCE_TYPE_SPARQL)) return true;
	if (language && semweb::isOntologyLanguageString(language.value())) return true;
	if (semweb::isTripleFormatString(format)) return true;
	return false;
}

DataSourcePtr DataSource::create(const VocabularyPtr &vocabulary, const boost::property_tree::ptree &config) {
	static const std::string formatDefault = {};

	// read data source settings
	URI dataSourceURI(config);
	auto dataSourceFormat = config.get(DATA_SOURCE_SETTING_FORMAT, formatDefault);
	auto o_dataSourceLanguage = config.get_optional<std::string>(DATA_SOURCE_SETTING_LANG);
	auto o_type = config.get_optional<std::string>(DATA_SOURCE_SETTING_TYPE);
	auto isOntology = isOntologySourceType(dataSourceFormat, o_dataSourceLanguage, o_type);
	// an optional frame can be applied to all triples in a data source
	auto o_tripleFrame = config.get_child_optional(DATA_SOURCE_SETTING_FRAME);
	std::shared_ptr<GraphSelector> tripleFrame;
	if (o_tripleFrame) {
		tripleFrame = std::make_shared<GraphSelector>();
		tripleFrame->set(*o_tripleFrame);
	}

	if (isOntology && o_type && o_type.value() == DATA_SOURCE_TYPE_SPARQL) {
		auto sparqlService = std::make_shared<SPARQLService>(dataSourceURI, dataSourceFormat);
		if (tripleFrame) {
			sparqlService->setFrame(tripleFrame);
		}
		return sparqlService;
	} else if (isOntology) {
		auto ontoFile = std::make_shared<OntologyFile>(vocabulary, dataSourceURI, dataSourceFormat);
		if (o_dataSourceLanguage.has_value()) {
			ontoFile->setOntologyLanguage(semweb::ontologyLanguageFromString(o_dataSourceLanguage.value()));
		}
		if (tripleFrame) {
			ontoFile->setFrame(tripleFrame);
		}
		return ontoFile;
	} else {
		return std::make_shared<DataSource>(dataSourceURI, dataSourceFormat, DataSourceType::UNSPECIFIED);
	}
}

namespace knowrob::py {
	template<>
	void createType<DataSource>() {
		using namespace boost::python;
		class_<DataSource, std::shared_ptr<DataSource>>
				("DataSource", no_init)
				.def("format", &DataSource::format, return_value_policy<copy_const_reference>())
				.def("uri", &DataSource::uri, return_value_policy<copy_const_reference>())
				.def("path", &DataSource::path, return_value_policy<copy_const_reference>())
				.def("version", &DataSource::version)
				.def("name", &DataSource::name);
	}
}


// fixture class for testing
namespace knowrob::testing {
	class DataSourceTest : public ::testing::Test {
	protected:
		void SetUp() override {}
		// void TearDown() override {}
	};
}
using namespace knowrob::testing;

TEST_F(DataSourceTest, IsGraphVersionString) {
	EXPECT_TRUE(DataSource::isVersionString("v1.1"));
	EXPECT_TRUE(DataSource::isVersionString("v10.1.54"));
	EXPECT_TRUE(DataSource::isVersionString("1.1"));
	EXPECT_TRUE(DataSource::isVersionString("10.1.54"));
	EXPECT_FALSE(DataSource::isVersionString("10"));
	EXPECT_FALSE(DataSource::isVersionString("x10.54.3"));
	EXPECT_FALSE(DataSource::isVersionString("x.y.z"));
}

TEST_F(DataSourceTest, GraphNameFromURI) {
	EXPECT_EQ(DataSource::getNameFromURI("https://www.ontologydesignpatterns.org/ont/dul/DUL.owl"), "DUL");
	EXPECT_EQ(DataSource::getNameFromURI("file:///owl/SOMA.owl"), "SOMA");
	EXPECT_EQ(DataSource::getNameFromURI("./ont/SOMA.owl"), "SOMA");
	EXPECT_EQ(DataSource::getNameFromURI("SOMA.owl"), "SOMA");
	EXPECT_EQ(DataSource::getNameFromURI("SOMA"), "SOMA");
}

TEST_F(DataSourceTest, GraphVersionFromURI) {
	EXPECT_EQ(DataSource::getVersionFromURI("https://foo/v1.2.2/owl"), "v1.2.2");
}
