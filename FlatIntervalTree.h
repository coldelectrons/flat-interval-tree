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


template <typename T, int ValueBits, int IntervalBits>
class FlatIntervalTree {
   public:
    const T min, max;
    const T fill;

    BEGIN_BITFIELD_TYPE( IntervalEntry, T )
    ADD_BITFIELD_MEMBER( lower, 0, IntervalBits )
    ADD_BITFIELD_MEMBER( value, IntervalBits + ValueBits, ValueBits )
    bool operator<( const IntervalEntry& rhs ) const
    {
        return lower < rhs.lower;
    }
    END_BITFIELD_TYPE()

    using self_type = FlatIntervalTree<T, ValueBits, IntervalBits>;
    using entry_type = IntervalEntry;
    using value_type = T;
    using tree_type = std::vector<entry_type>;
    using const_tree_iterator = typename tree_type::const_iterator;
    using tree_iterator = typename tree_type::iterator;
    using size_type = size_t;
    using difference_type = ptrdiff_t;
    using reference = T&;
    typedef T const_reference;
    typedef value_type* pointer;
    typedef const value_type* const_pointer;
    // typedef V value_type;
    // typedef iterator<T> iterator;
    // typedef std::reverse_iterator<const_iterator> const_reverse_iterator;
    // typedef std::reverse_iterator<iterator> reverse_iterator;
    tree_type tree;

    class const_iterator {
       public:
        using self_type = const_iterator;
        using value_type = T;
        using size_type = size_t;
        using difference_type = ptrdiff_t;
        using iterator_category = std::input_iterator_tag;
        using reference = T;
        using const_reference = const T;
        const_iterator( tree_type& tree, size_t tree_index, size_t position, size_t min,
                        size_t max )
            : tree(tree), i( tree_index ), pos( position ), min( min ), max( max )
        {
        }
        virtual ~const_iterator() {}
        self_type operator++()
        {
            ++pos;
            if ( pos >= tree[i+1]->lower ) {
                ++i;
            }
            return *this;
        }
        self_type operator--()
        {
            --pos;
            if ( pos < tree[i]->lower ) {
                --i;
            }
            return *this;
        }
        self_type operator+( ptrdiff_t n )
        {
            pos += n;

            while ( pos < tree[i]->lower ) {
                --i;
            }
            while ( pos >= tree[i + 1]->lower ) {
                ++i;
            }

            return *this;
        }
        self_type operator-( ptrdiff_t n )
        {
            pos -= n;

            while ( pos < tree[i]->lower ) {
                --i;
            }
            while ( pos >= tree[ i + 1 ]->lower ) {
                ++i;
            }

            return *this;
        }
        const_reference operator*() { return tree[i]->value; }
        const_reference operator->() { return tree[i]->value; }
        bool operator==( const self_type& rhs ) { return pos == rhs.pos; }
        bool operator!=( const self_type& rhs ) { return pos != rhs.pos; }
        difference_type distance( const self_type& rhs )
        {
            return rhs.pos - pos;
        }
        T Lower() { return tree[i]->lower; }
        T Upper() { return tree[i + 1 ]->lower; }
        T Value() { return tree[i]->value; }
       protected:
        // FlatIntervalTree& fit;
        tree_type& tree;
        size_t i;
        size_t pos;
        size_t min;
        size_t max;
    };

    FlatIntervalTree( T min, T max, T fill )
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
        const_iterator i( tree, 0, 0, min, max );
        return i;
    }
    const_iterator end() const
    {
        const_iterator i( tree, tree.size()-1, max, min, max );
        return i;
    }

    T at( size_t i ) const
    {
        return inInterval( i )->value;
    }
    T operator[]( size_t i ) const { return at( i ); }
    // size-1 due to sentinal
    size_t size() const { return tree.size() - 1; }
    // find the interval [l,u) that contains the point i
    const_iterator find( size_t i ) const
    {
        return std::move( iterator( inInterval( i ) ) );
    }

    bool is_sorted() const
    {
        return std::is_sorted(
            tree.begin(), tree.end(),
            []( const value_type& a, const value_type& b ) -> bool {
                return a.lower < b.lower;
            } );
    }

    // returns number of elements removed
    size_t uniqueify()
    {
        auto new_end = std::unique(
            tree.begin(), tree.end(),
            []( const value_type& a, const value_type& b ) -> bool {
                return a.value == b.value;
            } );
        auto rv = std::distance( new_end, tree.end() );
        tree.erase( new_end, tree.end() );
        return rv;
    }

    void reserve( size_t n ) { tree.reserve( n ); }
    size_t capacity() const { return tree.capacity(); }
    void write( size_t position, T value )
    {
        write( position, position + 1, value );
    }
    // complexity O(2*log(n)+2n)?
    /// writes/overwrites, adhering strictly to
    /// the rule of no adjacent intervals of like value
    void write( size_t lower, size_t upper, T value )
    {
        if ( lower < min ) {
            lower = min;
        }
        if ( upper > max ) {
            upper = max;
        }
        if ( lower >= upper ) {
            throw std::logic_error( "Improper interval !(lower<upper)" );
        }

        auto bit = inInterval( lower );
        auto eit = inInterval( upper );
        auto d = std::distance( bit, eit );
        if ( d == 0 ) {
            ++eit;
        }
        auto l = bit->lower;
        auto u = upper == max ? max : eit->lower;
        auto bvalue = bit->value;
        auto evalue = upper == max ? fill : eit->value;
        auto pvalue = bit == tree.begin() ? fill : ( bit - 1 )->value;
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
            // if value == bvalue then
            //   1) NoOp
            //      done
            // else if lower == l then
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
                    tmp.emplace_back( u, bvalue );
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
    self_type slice( size_t b, size_t e )
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
    std::pair<iterator, iterator> overlapping_range( size_t b, size_t e )
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

        return std::move( std::pair<iterator, iterator>(
            inInterval( b ), inInterval( e ) + 1 ) );
    }

   private:
    // Conduct binary search to find the highest lower bound
    // that is still lower than i.
    tree_iterator inInterval( size_t i )
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
        auto it = std::upper_bound(
            tree.begin(), tree.end(), entry_type(),
            [i]( const entry_type&, const entry_type& entry ) -> bool {
                return i < entry.lower;
            } );
        return --it;
    }
};
