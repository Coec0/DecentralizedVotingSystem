<template>
	<div class="results white">
		<h2>Results</h2>
		<highcharts v-if="voteCount > 0" :options="chartOptions"></highcharts>
		<div v-else>No results</div>
	</div>
</template>

<script>
import {Chart} from 'highcharts-vue'

export default {
	name: 'ResultPanel',
	components: {
		highcharts: Chart
	},
	props: {
		results: Array
	},
	computed: {
		voteCount: function () {
			return this.results.reduce((tot, cur) => { return tot += cur.y }, 0);
		}
	},
	data() {
		return {
			chartOptions: {
				chart: {
					plotBackgroundColor: null,
					plotBorderWidth: null,
					plotShadow: false,
					type: 'pie'
				},
				title: {
					text: null
				},
				series: [{
					name: 'Votes',
					data: this.results
				}]
			}
		}
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.results {
	min-height: 50px;
}
</style>
