#include <cassert>
#include <climits>
#include <cstdint>
#include <iostream>

#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_MODULE MyModule 
//#define BOOST_TEST_MAIN
#include <boost/test/unit_test.hpp>

#include "FlatIntervalTree.h"

//BOOST_AUTO_TEST_SUITE( flat_interval_tree_test_suite )

#define MIN 0
#define MAX 100
#define FILL 0

BOOST_AUTO_TEST_CASE( normal_ctor1 )
{
    FlatIntervalTree<IntervalEntry_u32_u32> t( MIN, MAX, FILL );

    for ( const auto& v: t.tree ) {
        BOOST_TEST_MESSAGE( "Entry: lower: " << v.lower << ", value:" << v.value);
    }

    BOOST_CHECK( t.tree.size() == 2 );
    BOOST_CHECK( t.max == MAX );
    BOOST_CHECK( t.min == MIN );
    BOOST_CHECK( t.fill == FILL );
    //BOOST_CHECK( t.tree.front().lower == 0 );
    //BOOST_CHECK( t.tree.front().value == 0 );
    //BOOST_CHECK( t.tree.back().lower == SIZE_1D );
    //BOOST_CHECK( t.tree.back().value == 0 );
}

BOOST_AUTO_TEST_CASE( write_single_1 )
{
    FlatIntervalTree<IntervalEntry_u32_u32> t( MIN, MAX, FILL );
    t.write(42, 1);

    BOOST_CHECK( t.tree.size() == 3 );
    BOOST_CHECK( t.tree.size() == 2 );
    BOOST_CHECK( t.max == MAX );
    BOOST_CHECK( t.min == MIN );
    BOOST_CHECK( t.fill == FILL );
    BOOST_CHECK( t.tree.front().lower == MIN );
    BOOST_CHECK( t.tree.front().value == FILL );
    BOOST_CHECK( t.tree.back().lower == MAX );
    BOOST_CHECK( t.tree.back().value == FILL );
}

// int main( int argc, char* argv[] )
//{
// std::vector<float> map( SIZE_1D, 0.0f );
// std::vector<uint32_t> imap( SIZE_1D, UINT_MAX );

// for ( size_t i = 0; i < SIZE_1D; i += 128 ) {
// for ( size_t j = 0; j < 64; ++j ) {
// imap[i + j] = 1;
// map[i + j] = 3;
//}
//}

// for ( size_t i = 0; i < SIZE_1D; i += 64 ) {
// for ( size_t j = 0; j < 64; ++j ) {
// if(imap[i + j] == 1) {
// std::cout << "1";
//} else {
// std::cout << "0";
//}
//}
// std::cout << "\n";
//}

// Plate p( MAPSIDE, map, imap, 1 );

// for ( auto& v: p.occupied.tree  ) {
// std::cout << v.value << ", " << v.lower << "\n";
//}
////for(auto pit = p.begin(); pit != p.end(); ++pit) {
////std::cout << *pit << " ";
////}

//{
// FlatIntervalTree<IntervalEntry_u32_u32> t( 0, SIZE_1D, false );
// for ( auto&& v : t.tree ) {
// std::cout << v.value << ", " << v.lower << "\n";
//}
// std::cout << "*****************************************\n";
// for ( size_t i = 0; i < SIZE_1D; i += 128 ) {
// for ( size_t j = 0; j < 64; ++j ) {
// t.write( i + j, true );
//[> code <]
//}
//}
// for ( auto&& v : t.tree ) {
// std::cout << v.value << ", " << v.lower << "\n";
//}
//}

// return 0;
//}
//BOOST_AUTO_TEST_SUITE_END()
