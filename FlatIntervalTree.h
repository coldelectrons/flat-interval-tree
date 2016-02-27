#pragma once

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <iterator>
#include <stdexcept>
#include <type_traits>
#include <vector>
#include "bitfield.h"

/**
  *
  * A map using an ordered vector of intervals, for representing
  * something analagous to Run-Length Encoding.
  *
  * The problem with RLE is that at any given point, you don't have much
  * state;  You know what you've got, but you don't know where you are.
  * Thus, you have to always forward-iterate to find a point in the stream.
  * Also, any insertion or change means propagating the change, if we are
  * going to maintain this as a virtual fixed-size array.
  *
  * This approach could be called Interval Encoding.  However, since I want
  * to keep the overhead down, our interval entry will not be
  *     std::pair<std::pair<uint,uint>, uint>,
  * but something like
  *     std::pair<uint,uint>
  *
  * With RLE, random access might be O(n/2) on average, with O(n) worst case.
  * With IE, random access should be O(log n), where n is the number of
  * intervals or runs in the stream.
  *
  * Forward iteration should perform similar to RLE with a custom iterator.
  *
  * Random access iterators for RLE are likely to be O(n) worst case.
  * Currently, my IE iterator is also O(n) worst case.
  *
  * Overwriting large areas of IE should perform much better, than RLE,
  * but I haven't benchmarked it.
  *
  * Proper operation presumes:
  * 1 - that the vector is always kept ordered.
  * 2 - that the intervals always cover the entire range of [IMin,IMax)
  * 3 - that the interval type is unsigned
  *
  * The interval type inside consists of a lower bound and a value type.
  * A given interval covers [lower, next lower)
  *
  * Due to the sheer difficulty of random access, all iterators should
  * be const:
  * - making a non-const proxy iterator is a bit beyond me right now
  * - writing through an iterator makes for a very hairy set of rules,
  *   and would likely invalidate the iterator anyway
  * - it would be far, far easier to simply const-iterate over one
  *   tree and write to another any changes.
  *
  * If I were using a pair<unsigned,unsigned>, then concievably
  * I could make this return a proper reference, but since I am
  * currently using a bitfield, that makes it not possible
  * (google issues with std::vector<bool>).
  *
  */

// TODO consider making the FIT templated on an external IntervalType,
// then make available here different types of intervals like
// bitfield and structs.
//
// Also, consider making min,max,fill all fixed a fixed size, either
// signed or unsigned, and make certain the algorithm works properly
// with that type.
//
// Since we are trying to emulate a a fixed-size array, it probably
// SHOULD be size_t or uint32_t

//BEGIN_BITFIELD_TYPE( IntervalEntry_bool_u31, uint32_t )
//ADD_BITFIELD_MEMBER( lower, 0, 31 )
//ADD_BITFIELD_MEMBER( value, 31, 1 )
//IntervalEntry_bool_u31( bool val, uint32_t low )
//{
    //value = val;
    //lower = low;
//}
//bool operator<( const IntervalEntry_bool_u31& rhs ) const
//{
    //return lower < rhs.lower;
//}
//END_BITFIELD_TYPE()

//BEGIN_BITFIELD_TYPE( IntervalEntry_u14_u18, uint32_t )
//ADD_BITFIELD_MEMBER( lower, 0, 18 )
//ADD_BITFIELD_MEMBER( value, 18, 14 )
//IntervalEntry_u14_u18( uint32_t val, uint32_t low )
//{
    //value = val;
    //lower = low;
//}
//bool operator<( const IntervalEntry_u14_u18& rhs ) const
//{
    //return lower < rhs.lower;
//}
//END_BITFIELD_TYPE()

//union IntervalEntry_u32_u32 {
    //uint64_t raw;
    //struct {
        //uint32_t value;
        //uint32_t lower;
    //};
    //IntervalEntry_u32_u32( uint32_t val, uint32_t low )
        //: value( val ), lower( low )
    //{
    //}
    //bool operator<( const IntervalEntry_u32_u32& rhs ) const
    //{
        //return lower < rhs.lower;
    //}
//};

template <class IntervalType>
class FlatIntervalTree {
    //static_assert( std::is_copy_constructible<IntervalType>::value,
                   //"Interval entry type must be copy constructible." );
    //static_assert( std::is_nothrow_move_constructible<IntervalType>::value &&
                       //std::is_nothrow_move_assignable<IntervalType>::value,
                   //"Swap may throw" );

   public:
    // the half-open bounds of this FIT, [min,max)
    const size_t min, max;
    // the default value for fill and sentinal values
    const size_t fill;

    //
    using self_type = FlatIntervalTree<IntervalType>;
    using interval_type = IntervalType;
    using tree_type = std::vector<interval_type>;
    using const_tree_iterator = typename tree_type::const_iterator;
    using tree_iterator = typename tree_type::iterator;
    using size_type = size_t;
    using difference_type = ptrdiff_t;
    using reference = interval_type&;
    using const_reference = const interval_type&;
    //
    tree_type tree;

    struct const_iterator {
       public:
        using self_type = const_iterator;
        using value_type = uint32_t;
        using size_type = size_t;
        using difference_type = ptrdiff_t;
        using iterator_category = std::input_iterator_tag;
        using reference = uint32_t;
        using const_reference = const uint32_t;
        // normal ctor
        const_iterator( tree_type* tree, size_t tree_index, size_t position )
            : tree( tree ), i( tree_index ), pos( position )
        {
        }
        const_iterator( const tree_type* tree, size_t tree_index,
                        size_t position )
            : tree( tree ), i( tree_index ), pos( position )
        {
        }
        virtual ~const_iterator() {}
        // copy ctor
        const_iterator( const self_type& rhs )
            : tree( rhs.tree ), i( rhs.i ), pos( rhs.pos )
        {
        }
        // move ctor
        const_iterator( self_type&& rhs )
            : tree( rhs.tree ), i( rhs.i ), pos( rhs.pos )
        {
        }
        // copy op
        void operator=( const self_type& rhs )
        {
            // is this iterator is already constructed by this FIT,
            // then 'tree' is already assigned and valid
            tree = rhs.tree;
            i = rhs.i;
            pos = rhs.pos;
        }
        // move op
        self_type& operator=( self_type&& rhs )
        {
            tree = rhs.tree;
            i = rhs.i;
            pos = rhs.pos;
            return *this;
        }
        self_type operator++()
        {
            ++pos;
            if ( pos >= ( *tree )[i + 1].lower ) {
                ++i;
            }
            return *this;
        }
        self_type operator--()
        {
            --pos;
            if ( pos < ( *tree )[i].lower ) {
                --i;
            }
            return *this;
        }
        self_type operator+( ptrdiff_t n )
        {
            pos += n;

            while ( pos < ( *tree )[i].lower ) {
                --i;
            }
            while ( pos >= ( *tree )[i + 1].lower ) {
                ++i;
            }

            return *this;
        }
        self_type operator-( ptrdiff_t n )
        {
            pos -= n;

            while ( pos < ( *tree )[i].lower ) {
                --i;
            }
            while ( pos >= ( *tree )[i + 1].lower ) {
                ++i;
            }

            return *this;
        }
        const_reference operator*() { return ( *tree )[i]->value; }
        const_reference operator->() { return ( *tree )[i]->value; }
        bool operator==( const self_type& rhs ) { return pos == rhs.pos; }
        bool operator!=( const self_type& rhs ) { return pos != rhs.pos; }
        difference_type distance( const self_type& rhs )
        {
            return rhs.pos - pos;
        }
        uint32_t Lower() { return ( *tree )[i].lower; }
        uint32_t Upper() { return ( *tree )[i + 1].lower; }
        uint32_t Value() { return ( *tree )[i].value; }
        tree_type const* tree;
        size_t i;
        size_t pos;
    };

    FlatIntervalTree( uint32_t min, uint32_t max, uint32_t fill )
        : min( min ), max( max ), fill( fill )
    {
        // sentinal values
        tree.emplace_back( min, fill );
        tree.emplace_back( max, fill );
    }
    FlatIntervalTree( const FlatIntervalTree& that ) noexcept
        : min( that.min ),
          max( that.max ),
          fill( that.fill ),
          tree( that.tree )
    {
    }
    FlatIntervalTree( FlatIntervalTree&& that ) noexcept
        : min( std::move( that.min ) ),
          max( std::move( that.max ) ),
          fill( std::move( that.fill ) ),
          tree( std::move( that.tree ) )
    {
    }

    virtual ~FlatIntervalTree() {}
    const_iterator begin() const
    {
        const_iterator i( &tree, 0, 0 );
        return i;
    }
    const_iterator end() const
    {
        const_iterator i( &tree, tree.size() - 1, max );
        return i;
    }

    /// returns the value in interval i
    uint32_t at( size_t i ) const { return inInterval( i )->value; }
    /// returns the value in interval i
    uint32_t operator[]( size_t i ) const { return at( i ); }
    /// returns the virtual number of elements
    size_t size() const { return max - min; }
    /// find the first point with value v.
    const_iterator find_value( uint32_t v ) const
    {
        const_iterator it = end();
        auto vit = std::find_if(
            tree.begin(), tree.end(),
            [=]( const interval_type& i ) -> bool { return i.value == v; } );
        if ( vit != tree.end() ) {
            it.i = std::distance( tree.begin(), vit );
            it.pos = vit->lower;
        }
        return it;
    }

    /// search for value v starting at iterator i.
    const_iterator find_value( const_iterator sit, uint32_t v ) const
    {
        auto it = end();
        auto vit = std::find_if(
            tree.begin() + sit.i, tree.end(),
            [=]( const interval_type& i ) -> bool { return i.value == v; } );
        if ( vit != tree.end() ) {
            it.i = std::distance( tree.begin(), vit );
            it.pos = vit->lower;
        }
        return it;
    }

    /// return iterator to the interval [l,u) that contains the point i
    const_iterator find_interval( uint32_t i ) const
    {
        const auto it = inInterval( i );
        return const_iterator( &tree, std::distance( tree.begin(), it ),
                               it->lower );
    }

    bool is_sorted() const
    {
        return std::is_sorted(
            tree.begin(), tree.end(),
            []( const interval_type& a, const interval_type& b ) -> bool {
                return a.lower < b.lower;
            } );
    }

    // returns number of elements removed
    size_t uniqueify()
    {
        auto new_end = std::unique(
            tree.begin(), tree.end(),
            []( const interval_type& a, const interval_type& b ) -> bool {
                return a.value == b.value;
            } );
        auto rv = std::distance( new_end, tree.end() );
        tree.erase( new_end, tree.end() );
        return rv;
    }

    void reserve( size_t n ) { tree.reserve( n ); }
    size_t capacity() const { return tree.capacity(); }
    void write( uint32_t position, uint32_t value )
    {
        write( position, position + 1, value );
    }
    // complexity O(2*log(n)+2n)?
    /// writes/overwrites, adhering strictly to
    /// the rule of no adjacent intervals of like value
    void write( const uint32_t l_, const uint32_t u_, const uint32_t value )
    {
        const uint32_t lower = l_ < min ? min : l_;
        const uint32_t upper = u_ > max ? max : u_;
        if ( lower >= upper ) {
            throw std::logic_error( "Improper interval !(lower<upper)" );
        }

        const auto bit = inInterval( lower );
        const auto tmp = inInterval( upper );
        // if the interval we are writing exists entirely within one interval
        // increment eit so we can access the upper bound info
        const auto d = std::distance( bit, tmp );
        const auto eit = d==0 ? tmp+1 : tmp;
        const auto l = bit->lower;
        const auto u = upper == max ? max : eit->lower;
        const auto bvalue = bit->value;
        const auto evalue = upper == max ? fill : eit->value;
        const auto pvalue = bit == tree.begin() ? fill : ( bit - 1 )->value;
        // Cheap In-place Operations:
        // Try to identify cases where work need not be done
        // or elements can be modified in-place.
        // Then identify simple cases where the tree can be modified
        // in-place, but may still need emplacement or erasure.
        // Cases that need erasure or emplacement should be attempted to be
        // generalized to d==N to avoid unnecessary code.

        if ( d == 0 ) {
            // d==0 - when both iterators are in the same interval
            // implicitly, upper < u
            //          l                 u
            //   ppppppp[bbbbbbbbbbbbbbbbb[eeeeeeeeee
            // 1        [bbbbbbbb
            // 1            [bbbbbbbb
            // 2        [pppppppp
            // 3        [vvvvvvvv
            // 4            [vvvvvvvv
            //
            // if our value matches the existing entry value
            //   1) NoOp
            //      done
            //
            // if lower == l 
            // ( our interval entry starts on the same point as the existing )
            //   if b != begin then
            //     if value == pvalue then
            //       2) GrowBeforeB
            //          modify b.lower = upper
            //          done
            //     else (value != pvalue)
            //       3) InsertBeforeB
            //          we modify b and emplace our new
            //          element before it.
            //          done
            //   else (b == begin)
            //     3) InsertBeforeB
            //        we modify b and emplace our new
            //        element before it.
            //        done
            // else (lower != l)
            //   4) InsertTwoBeforeB
            //      we modify b and create a tmp with out two elements
            //      and insert them before b
            //
            if ( value == bvalue ) {
                // 1) NoOp
                return;
            }
            else {
                if ( lower == l ) {
                    if ( bit != tree.begin() ) {
                        auto pvalue = ( bit - 1 )->value;
                        if ( value == pvalue ) {
                            // 2) GrowBeforeB
                            // modify bit
                            bit->lower = upper;
                            return;
                        }
                        else {
                            // 3) InsertBeforeB
                            // modify bit
                            bit->lower = upper;
                            // emplace 1 element before bit
                            tree.emplace( bit, lower, value );
                            return;
                        }
                    }
                    else {
                        // 3) InsertBeforeB
                        // modify bit
                        bit->lower = upper;
                        // emplace 1 element before bit
                        tree.emplace( bit, lower, value );
                        return;
                    }
                }
                else if ( lower > l ) {
                    // 4) InsertTwoBeforeB
                    // modify bit
                    bit->lower = upper;
                    // use a tmp, so we only need to do 1 copy/move, not 2
                    tree_type tmp;
                    tmp.emplace_back( l, bvalue );
                    tmp.emplace_back( lower, value );
                    tree.insert( bit, tmp.begin(), tmp.end() );
                    return;
                }
            }
        }
        if ( lower == l ) {
            if ( upper == u ) {
                // lower == l, upper == u
                //
                //            l                 u
                // A
                // d1  ppppppp[xxxxxxxxxxxxxxxxx[eeeeeeeeee
                // d2  ppppppp[xxxxxxxx[xxxxxxxx[eeeeeeeeee
                // dn  ppppppp[xxxxx[xxx[xxxxxxx[eeeeeeeeee
                //            [vvvvvvvvvvvvvvvvv
                //
                // B
                // d1  vvvvvvv[xxxxxxxxxxxxxxxxx[eeeeeeeeee
                // d2  vvvvvvv[xxxxxxxx[xxxxxxxx[eeeeeeeeee
                // dn  vvvvvvv[xxxxx[xxx[xxxxxxx[eeeeeeeeee
                // d1         [xxxxxxxxxxxxxxxxx[eeeeeeeeee
                // d2         [xxxxxxxx[xxxxxxxx[eeeeeeeeee
                // dn         [xxxxx[xxx[xxxxxxx[eeeeeeeeee
                //            [vvvvvvvvvvvvvvvvv
                //
                // C
                // d1  ppppppp[xxxxxxxxxxxxxxxxx[vvvvvvvvvv
                // d2  ppppppp[xxxxxxxx[xxxxxxxx[vvvvvvvvvv
                // dn  ppppppp[xxxxx[xxx[xxxxxxx[vvvvvvvvvv
                // d1  ppppppp[xxxxxxxxxxxxxxxxx[sentinel
                // d2  ppppppp[xxxxxxxx[xxxxxxxx[sentinel
                // dn  ppppppp[xxxxx[xxx[xxxxxxx[sentinel
                //            [vvvvvvvvvvvvvvvvv
                //
                // D
                // d1  vvvvvvv[xxxxxxxxxxxxxxxxx[vvvvvvvvvv
                // d2  vvvvvvv[xxxxxxxx[xxxxxxxx[vvvvvvvvvv
                // dn  vvvvvvv[xxxxx[xxx[xxxxxxx[vvvvvvvvvv
                // d1         [xxxxxxxxxxxxxxxxx[vvvvvvvvvv
                // d2         [xxxxxxxx[xxxxxxxx[vvvvvvvvvv
                // dn         [xxxxx[xxx[xxxxxxx[vvvvvvvvvv
                // d1  vvvvvvv[xxxxxxxxxxxxxxxxx[sentinel
                // d2  vvvvvvv[xxxxxxxx[xxxxxxxx[sentinel
                // dn  vvvvvvv[xxxxx[xxx[xxxxxxx[sentinel
                // d1         [xxxxxxxxxxxxxxxxx[sentinel
                // d2         [xxxxxxxx[xxxxxxxx[sentinel
                // dn         [xxxxx[xxx[xxxxxxx[sentinel
                //            [vvvvvvvvvvvvvvvvv
                if ( value != pvalue && value != evalue ) {
                    bit->value = value;
                    tree.erase( bit + 1, eit );
                    return;
                }
                if ( value == pvalue && value != evalue ) {
                    if ( bit != tree.begin() ) {
                        tree.erase( bit, eit );
                    }
                    else {
                        bit->value = value;
                        tree.erase( bit + 1, eit );
                    }
                    return;
                }
                if ( value != pvalue && value == evalue ) {
                    if ( upper != max ) {
                        eit->lower = lower;
                        tree.erase( bit, eit );
                    }
                    else {
                        bit->value = value;
                        tree.erase( bit + 1, eit );
                    }
                    return;
                }
                if ( value == pvalue && value == evalue ) {
                    if ( bit != tree.begin() ) {
                        if ( upper != max ) {
                            tree.erase( bit, eit + 1 );
                        }
                        else {
                            tree.erase( bit, eit );
                        }
                    }
                    else {
                        if ( upper != max ) {
                            eit->lower = lower;
                            tree.erase( bit + 1, eit );
                        }
                        else {
                            bit->value = value;
                            tree.erase( bit + 1, eit );
                        }
                    }
                    return;
                }
            }
            else if ( upper > u ) {
                // lower == l, upper > u
                //
                //            l                 u
                // A
                // d1  ppppppp[xxxxxxxxxxxxxxxxx[eeeeeeeeee
                // d2  ppppppp[xxxxxxxx[xxxxxxxx[eeeeeeeeee
                // dn  ppppppp[xxxxx[xxx[xxxxxxx[eeeeeeeeee
                //            [vvvvvvvvvvvvvvvvvvvvvvv
                //
                // B
                // d1  vvvvvvv[xxxxxxxxxxxxxxxxx[eeeeeeeeee
                // d2  vvvvvvv[xxxxxxxx[xxxxxxxx[eeeeeeeeee
                // dn  vvvvvvv[xxxxx[xxx[xxxxxxx[eeeeeeeeee
                // d1         [xxxxxxxxxxxxxxxxx[eeeeeeeeee
                // d2         [xxxxxxxx[xxxxxxxx[eeeeeeeeee
                // dn         [xxxxx[xxx[xxxxxxx[eeeeeeeeee
                //            [vvvvvvvvvvvvvvvvvvvvvvv
                //
                // C
                // d1  ppppppp[xxxxxxxxxxxxxxxxx[vvvvvvvvvv
                // d2  ppppppp[xxxxxxxx[xxxxxxxx[vvvvvvvvvv
                // dn  ppppppp[xxxxx[xxx[xxxxxxx[vvvvvvvvvv
                //            [vvvvvvvvvvvvvvvvvvvvvvv
                //
                // D
                // d1  vvvvvvv[xxxxxxxxxxxxxxxxx[vvvvvvvvvv
                // d2  vvvvvvv[xxxxxxxx[xxxxxxxx[vvvvvvvvvv
                // dn  vvvvvvv[xxxxx[xxx[xxxxxxx[vvvvvvvvvv
                // d1         [xxxxxxxxxxxxxxxxx[vvvvvvvvvv
                // d2         [xxxxxxxx[xxxxxxxx[vvvvvvvvvv
                // dn         [xxxxx[xxx[xxxxxxx[vvvvvvvvvv
                //            [vvvvvvvvvvvvvvvvvvvvvvv
                if ( value != pvalue && value != evalue ) {
                    bit->value = value;
                    eit->lower = upper;
                    tree.erase( bit + 1, eit );
                    return;
                }
                if ( value == pvalue && value != evalue ) {
                    eit->lower = upper;
                    if ( bit != tree.begin() ) {
                        tree.erase( bit, eit );
                    }
                    else {
                        bit->value = value;
                        tree.erase( bit + 1, eit );
                    }
                    return;
                }
                if ( value != pvalue && value == evalue ) {
                    eit->lower = lower;
                    tree.erase( bit, eit );
                    return;
                }
                if ( value == pvalue && value == evalue ) {
                    if ( bit != tree.begin() ) {
                        tree.erase( bit, eit + 1 );
                    }
                    else {
                        bit->value = value;
                        tree.erase( bit + 1, eit );
                    }
                    return;
                }
            }
        }
        else if ( lower > l ) {
            if ( upper == u ) {
                // lower > l, upper == u
                //
                //            l                 u
                // A
                // d1         [vvvvvvvvvvvvvvvvv[eeeeeeeeee
                // d2         [vvvvvvvv[xxxxxxxx[eeeeeeeeee
                // dn         [vvvvv[xxx[xxxxxxx[eeeeeeeeee
                //               [vvvvvvvvvvvvvv
                // B
                // d1         [bbbbbbbbbbbbbbbbb[eeeeeeeeee
                // d2         [bbbbbbbb[xxxxxxxx[eeeeeeeeee
                // dn         [bbbbb[xxx[xxxxxxx[eeeeeeeeee
                //               [vvvvvvvvvvvvvv
                // C
                // d1         [bbbbbbbbbbbbbbbbb[vvvvvvvvvv
                // d2         [bbbbbbbb[xxxxxxxx[vvvvvvvvvv
                // dn         [bbbbb[xxx[xxxxxxx[vvvvvvvvvv
                // d1         [bbbbbbbbbbbbbbbbb[sentinel
                // d2         [bbbbbbbb[xxxxxxxx[sentinel
                // dn         [bbbbb[xxx[xxxxxxx[sentinel
                //               [vvvvvvvvvvvvvv
                // D
                // d1         [vvvvvvvvvvvvvvvvv[vvvvvvvvvv
                // d2         [vvvvvvvv[xxxxxxxx[vvvvvvvvvv
                // dn         [vvvvv[xxx[xxxxxxx[vvvvvvvvvv
                // d1         [vvvvvvvvvvvvvvvvv[sentinel
                // d2         [vvvvvvvv[xxxxxxxx[sentinel
                // dn         [vvvvv[xxx[xxxxxxx[sentinel
                //               [vvvvvvvvvvvvvv
                if ( value == bvalue && value != evalue ) {
                    tree.erase( bit + 1, eit );
                    return;
                }
                if ( value != bvalue && value != evalue ) {
                    if ( d == 1 ) {
                        tree.emplace( eit, lower, value );
                    }
                    else if ( d >= 2 ) {
                        auto mit = bit + 1;
                        mit->lower = lower;
                        mit->value = value;
                        tree.erase( mit + 1, eit );
                    }
                    return;
                }
                if ( value != bvalue && value == evalue ) {
                    if ( upper != max ) {
                        eit->lower = lower;
                        tree.erase( bit + 1, eit );
                    }
                    else {
                        if ( d == 1 ) {
                            tree.emplace( eit, lower, value );
                        }
                        else if ( d >= 2 ) {
                            auto mit = bit + 1;
                            mit->lower = lower;
                            mit->value = value;
                            tree.erase( mit + 1, eit );
                        }
                    }
                    return;
                }
                if ( value == bvalue && value == evalue ) {
                    if ( upper != max ) {
                        if ( d == 1 ) {
                            tree.erase( eit );
                        }
                        else if ( d >= 2 ) {
                            tree.erase( bit + 1, eit + 1 );
                        }
                    }
                    else {
                        if ( d == 1 ) {
                            return;
                        }
                        else if ( d >= 2 ) {
                            tree.erase( bit + 1, eit );
                        }
                    }
                    return;
                }
            }
            else if ( upper > u ) {
                // lower > l, upper > u
                //
                //            l                 u
                // A
                // d1         [vvvvvvvvvvvvvvvvv[eeeeeeeeee
                // d2         [vvvvvvvv[xxxxxxxx[eeeeeeeeee
                // dn         [vvvvv[xxx[xxxxxxx[eeeeeeeeee
                //               [vvvvvvvvvvvvvvvvvv
                // B
                // d1         [bbbbbbbbbbbbbbbbb[eeeeeeeeee
                // d2         [bbbbbbbb[xxxxxxxx[eeeeeeeeee
                // dn         [bbbbb[xxx[xxxxxxx[eeeeeeeeee
                //               [vvvvvvvvvvvvvvvvvv
                // C
                // d1         [bbbbbbbbbbbbbbbbb[vvvvvvvvvv
                // d2         [bbbbbbbb[xxxxxxxx[vvvvvvvvvv
                // dn         [bbbbb[xxx[xxxxxxx[vvvvvvvvvv
                //               [vvvvvvvvvvvvvvvvvv
                // D
                // d1         [vvvvvvvvvvvvvvvvv[vvvvvvvvvv
                // d2         [vvvvvvvv[xxxxxxxx[vvvvvvvvvv
                // dn         [vvvvv[xxx[xxxxxxx[vvvvvvvvvv
                //               [vvvvvvvvvvvvvvvvvv
                if ( value == bvalue && value != evalue ) {
                    eit->lower = upper;
                    tree.erase( bit + 1, eit );
                    return;
                }
                if ( value != bvalue && value != evalue ) {
                    if ( d == 1 ) {
                        eit->lower = upper;
                        tree.emplace( eit, lower, value );
                    }
                    else if ( d >= 2 ) {
                        eit->lower = upper;
                        auto mit = bit + 1;
                        mit->lower = lower;
                        mit->value = value;
                        tree.erase( mit + 1, eit );
                    }
                    return;
                }
                if ( value != bvalue && value == evalue ) {
                    eit->lower = lower;
                    tree.erase( bit + 1, eit );
                    return;
                }
                if ( value == bvalue && value == evalue ) {
                    if ( d == 1 ) {
                        tree.erase( eit );
                    }
                    else if ( d >= 2 ) {
                        tree.erase( bit + 1, eit + 1 );
                    }
                    return;
                }
            }
        }

        return;
    }

    // Complexity O(2*log(n))?
    self_type slice( uint32_t b, uint32_t e )
    {
        //
        self_type rv( b, e, fill );
        if ( b < min ) {
            b = min;
        }
        if ( e > max ) {
            e = max;
        }
        if ( b >= e ) {
            throw std::logic_error( "Bad half-open interval" );
        }

        auto bit = inInterval( b );
        auto eit = inInterval( e ) + 1;

        rv.tree.reserve( std::distance( bit, eit ) + 1 );

        if ( bit == eit ) {
            throw std::logic_error( "Error slicing tree:  bit==eit" );
            // std::cout << "Bad slice?: bit == eit\n";
        }

        // clear default sentinal value
        rv.tree.clear();
        // place front value (possibly nipped off)
        rv.tree.emplace_back( b, bit->value );
        ++bit;
        // copy the rest
        for ( ; bit != eit; ++bit ) {
            rv.tree.insert( bit, eit, rv.tree.end() );
        }
        // make sure we have the sentinal
        if ( rv.tree.back().lower != e ) {
            rv.tree.emplace_back( e, fill );
        }

        return std::move( rv );
    }

    // get the range of intervals the given half-open interval overlaps
    std::pair<const_iterator, const_iterator> overlapping_range( uint32_t b,
                                                                 uint32_t e )
    {
        //
        if ( b < min ) {
            b = min;
        }
        if ( e > max ) {
            e = max;
        }
        if ( b >= e ) {
            throw std::logic_error( "Bad half-open interval" );
        }

        return std::move( std::pair<const_iterator, const_iterator>(
            inInterval( b ), inInterval( e ) + 1 ) );
    }

   private:
    // Conduct binary search to find the highest lower bound
    // that is still lower than i.
    tree_iterator inInterval( uint32_t i )
    {
        // if ( i >= max || i < min ) {
        // throw std::out_of_range( "Cannot find i in [min,max)" );
        //// return tree.end();
        //}
        if ( i <= min ) {
            return tree.begin();
        }
        if ( i >= max ) {
            return tree.end() - 1;
        }
        // XXX TODO look harder at why this works
        auto it = std::lower_bound(
            tree.rbegin(), tree.rend(), interval_type( i, i ),
            []( const interval_type& l, const interval_type& entry ) -> bool {
                return l.lower > entry.lower;
            } );
        //assert( it != tree.rend() );
        return it.base() - 1;
    }
};

// FlatIntervalTree<IntervalEntry_u14_u18> fit14(0,64*64*64,0);
// FlatIntervalTree<IntervalEntry_u32_u32> fit32(0,64*64*64,0);

// int main( int argc, char *argv[] )
//{
// fit14.is_sorted();
// fit14.write(3162,13414,23);
// return 0;
//}
