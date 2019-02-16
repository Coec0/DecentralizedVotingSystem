<template>
	<div class="button primary-color">
		<router-link v-bind:to="to" tag="div">{{ text }}</router-link>
	</div>
</template>

<script>
export default {
	name: 'NavButton',
	props: {
		text: String,
		to: String
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.button {
	width: 100px;
	height: 50px;
	-webkit-transition-duration: 0.4s; /* Safari */
	transition-duration: 0.4s;
}
.button:hover {
	color: white;
	background-color: #2c3e50;
	cursor: default;
	filter: brightness(85%);
}

.button div {
	height: 100%;
	display: flex;
	justify-content: center; /* align horizontal */
	align-items: center; /* align vertical */
}

.button .router-link-active {
	color: white;
}
</style>
